%%
%% pivot_clients_api_events_handler.erl
%%
-module(pivot_clients_api_events_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("pivot_clients_api.hrl").

-define(TRANSPARENT_GIF, <<71,73,70,56,57,97,1,0,1,0,128,0,0,0,0,0,255,255,255,33,249,4,1,0,0,0,0,44,0,0,0,0,1,0,1,0,0,2,1,68,0,59>>).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = check_method(Method, Req2),
  {ok, Req3, State}.

check_method(<<"GET">>, Req) ->
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Event, Req3} = cowboy_req:qs_val(<<"e">>, Req2),
  {UserID, Req4} = cowboy_req:qs_val(<<"u">>, Req3),
  {Version, Req4} = cowboy_req:qs_val(<<"v">>, Req3, <<"*">>),
  maybe_track(App, Event, UserID, Version, Req4);
check_method(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),
  App = fast_key:get(<<"a">>, Params),
  Event = fast_key:get(<<"e">>, Params),
  UserID = fast_key:get(<<"u">>, Params),
  Version = fast_key:get(<<"v">>, Params, <<"*">>),
  maybe_track(App, Event, UserID, Version, Req2);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybe_track(undefined, _, _, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, 400, Req);
maybe_track(_, undefined, _, _, Req) ->
  ?ERROR(<<"Missing event (e) parameter.">>, 400, Req);
maybe_track(_, _, undefined, _, Req) ->
  ?ERROR(<<"Missing user id (u) parameter.">>, 400, Req);
maybe_track(App, Event, UserID, Version, Req) ->
  RequestID = cowboy_request_id:get(Req),
  PReq = #pivot_req{
    id = RequestID,
    env = cowboy_env:get(Req),
    app = App,
    version = Version,
    user = UserID,
    event = Event
  },

  pivot_client:do_async(events, add, PReq),

  cowboy_req:reply(200, [
    {<<"content-type">>, <<"image/gif">>},
    {<<"cache-control">>, <<"no-cache, max-age=0">>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-max-age">>, <<"31536000">>},
    {<<"x-request-id">>, RequestID}
  ], ?TRANSPARENT_GIF, Req).

terminate(_Reason, _Req, _State) ->
  ok.
