%%
%% pivot_clients_api_assignments_handler.erl
%%
-module(pivot_clients_api_assignments_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("pivot_clients_api.hrl").

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = check_method(Method, Req2),
  {ok, Req3, State}.

check_method(<<"GET">>, Req) ->
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Version, Req3} = cowboy_req:qs_val(<<"v">>, Req2, <<"*">>),
  maybe_assign(App, Version, Req3);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybe_assign(undefined, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, 400, Req);
maybe_assign(App, Version, Req) ->
  RequestID = cowboy_request_id:get(Req),

  PReq = pivot:req([
    {id, RequestID},
    {env, cowboy_env:get(Req)},
    {app, App},
    {version, Version}
  ]),

  case pivot:do(assignments, assign, PReq) of
    {ok, Assignments, Token, TTL} ->
      respond(Assignments, Token, TTL, RequestID, Req);
    {error, notfound} ->
      respond(3600, RequestID, Req);
    Error ->
      ?ERROR(io_lib:format("~p~n", [Error]), 500, Req)
  end.

respond(TLL, RequestID, Req) ->
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json">>},
    {<<"cache-control">>, <<"public, max-age=", (integer_to_binary(TLL))/binary>>},
    {<<"x-request-id">>, RequestID}
  ], <<"{\"assignments\":{}}">>, Req).

respond(Assignments, Token, TLL, RequestID, Req) ->
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json">>},
    {<<"cache-control">>, <<"public, max-age=", (integer_to_binary(TLL))/binary>>},
    {<<"x-request-id">>, RequestID}
  ], jsx:encode([
    {<<"assignments">>, Assignments},
    {<<"token">>, Token}
  ]), Req).

terminate(_Reason, _Req, _State) ->
  ok.
