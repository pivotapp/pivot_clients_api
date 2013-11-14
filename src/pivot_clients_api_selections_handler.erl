%%
%% pivot_clients_api_selections_handler.erl
%%
-module(pivot_clients_api_selections_handler).

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
  Env = cowboy_env:get(Req),
  RequestID = cowboy_request_id:get(Req),
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Version, Req3} = cowboy_req:qs_val(<<"v">>, Req2, <<"*">>),
  PReq = pivot_client:new([
    {id, RequestID},
    {env, Env},
    {app, App},
    {version, Version}
  ]),
  case pivot_client:do(selections, get, PReq) of
    {ok, Selections} ->
      reply(jsx:encode(Selections), Req3)
  end;
check_method(<<"PUT">>, Req) ->
  Env = cowboy_env:get(Req),
  RequestID = cowboy_request_id:get(Req),
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Version, Req3} = cowboy_req:qs_val(<<"v">>, Req2, <<"*">>),
  {ok, Selections, Req4} = cowboy_req:body(Req3),
  %% TODO support more than one content type
  % case catch binary_to_term(Selections) of
  case catch jsx:decode(Selections) of
    Parsed when is_list(Parsed) ->
      PReq = pivot_client:new([
        {id, RequestID},
        {env, Env},
        {app, App},
        {version, Version},
        {selections, Parsed}
      ]),
      ok =  pivot_client:do(selections, set, PReq),
      reply(Selections, Req4);
    _ ->
      ?ERROR(<<"Invalid selections value">>, 400, Req)
  end;
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.

reply(Selections, Req) ->
  cowboy_req:reply(200, [
    {<<"cache-control">>, <<"max-age=5">>}
  ], Selections, Req).
