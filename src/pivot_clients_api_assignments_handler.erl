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
  {UserID, Req3} = cowboy_req:qs_val(<<"u">>, Req2),
  {Version, Req4} = cowboy_req:qs_val(<<"v">>, Req3, <<"*">>),
  maybe_assign(App, UserID, Version, Req4);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybe_assign(undefined, _, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, 400, Req);
maybe_assign(_, undefined, _, Req) ->
  ?ERROR(<<"Missing user id (u) parameter.">>, 400, Req);
maybe_assign(App, UserID, Version, Req) ->
  RequestID = cowboy_request_id:get(Req),

  PReq = #pivot_req{
    id = RequestID,
    env = cowboy_env:get(Req),
    app = App,
    version = Version,
    user = UserID
  },

  case pivot_client:do(assignments, assign, PReq) of
    {ok, []} ->
      respond([], 3600, RequestID, Req);
    {ok, Assignments} ->
      respond(Assignments, 86400, RequestID, Req);
    Error ->
      cowboy_req:reply(500, [
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-max-age">>, <<"31536000">>},
        {<<"x-request-id">>, RequestID}
      ], term_to_binary(Error), Req)
  end.

respond(Assignments, TLL, RequestID, Req) ->
  {Content, Type, Req3} = case cowboy_req:qs_val(<<"callback">>, Req) of
    {undefined, Req2} ->
      {encode(Assignments), <<"application/json">>, Req2};
    {Callback, Req2} ->
      {[Callback, <<"(">>, encode(Assignments), <<");">>],
       <<"application/javascript">>, Req2}
  end,

  cowboy_req:reply(200, [
    {<<"content-type">>, Type},
    {<<"cache-control">>, <<"public, max-age=", (integer_to_binary(TLL))/binary>>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-max-age">>, <<"31536000">>},
    {<<"x-request-id">>, RequestID}
  ], Content, Req3).

encode([]) ->
  <<"{}">>;
encode(Assignments) ->
  jsx:encode(Assignments).

terminate(_Reason, _Req, _State) ->
  ok.
