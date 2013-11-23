%%
%% pivot_clients_api_events_handler.erl
%%
-module(pivot_clients_api_events_handler).

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
  {Event, Req3} = cowboy_req:qs_val(<<"e">>, Req2),
  {Token, Req4} = cowboy_req:qs_val(<<"t">>, Req3),
  {Version, Req5} = cowboy_req:qs_val(<<"v">>, Req4, <<"*">>),
  {Test, Req6} = cowboy_req:qs_val(<<"test">>, Req5),
  maybe_track(App, Event, Token, Version, Test, Req6);
check_method(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),
  App = fast_key:get(<<"a">>, Params),
  Event = fast_key:get(<<"e">>, Params),
  Token = fast_key:get(<<"u">>, Params),
  Version = fast_key:get(<<"v">>, Params, <<"*">>),
  Test = fast_key:get(<<"test">>, Params),
  maybe_track(App, Event, Token, Version, Test, Req2);
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

maybe_track(undefined, _, _, _, _, Req) ->
  ?ERROR(<<"Missing app (a) parameter.">>, 400, Req);
maybe_track(_, undefined, _, _, _, Req) ->
  ?ERROR(<<"Missing event (e) parameter.">>, 400, Req);
maybe_track(_, _, undefined, _, _, Req) ->
  ?ERROR(<<"Missing token (t) parameter.">>, 400, Req);
maybe_track(_, _, _, _, <<"1">>, Req) ->
  reply(Req);
maybe_track(App, Event, Token, Version, undefined, Req) ->
  RequestID = cowboy_request_id:get(Req),

  PReq = pivot:req([
    {id, RequestID},
    {env, cowboy_env:get(Req)},
    {app, App},
    {version, Version},
    {token, Token},
    {event, Event}
  ]),

  pivot_client:do_async(events, add, PReq),
  reply(Req).

reply(Req) ->
  RequestID = cowboy_request_id:get(Req),
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"image/gif">>},
    {<<"cache-control">>, <<"no-cache, max-age=0">>},
    {<<"x-request-id">>, RequestID}
  ], ?TRANSPARENT_GIF, Req).

terminate(_Reason, _Req, _State) ->
  ok.
