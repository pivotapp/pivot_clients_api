%%
%% pivot_clients_api_rewards_handler.erl
%%
-module(pivot_clients_api_rewards_handler).

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
  {Event, Req3} = cowboy_req:qs_val(<<"e">>, Req2),
  PReq = pivot_client:new([
    {id, RequestID},
    {env, Env},
    {app, App},
    {event, Event}
  ]),

  case pivot_client:do(rewards, get, PReq) of
    {ok, Reward} when is_binary(Reward) ->
      reply(Reward, Req3);
    {ok, Reward} ->
      reply(term_to_binary(Reward), Req3);
    {error, notfound} ->
      ?ERROR(<<"Event not found">>, 404, Req)
  end;
check_method(<<"PUT">>, Req) ->
  Env = cowboy_env:get(Req),
  RequestID = cowboy_request_id:get(Req),
  {App, Req2} = cowboy_req:qs_val(<<"a">>, Req),
  {Event, Req3} = cowboy_req:qs_val(<<"e">>, Req2),
  {ok, Reward, Req4} = cowboy_req:body(Req3),

  case catch binary_to_float(Reward) of
    Parsed when 0.0 =< Parsed, 1.0 >= Parsed ->
      PReq = pivot_client:new([
        {id, RequestID},
        {env, Env},
        {app, App},
        {event, Event},
        {reward, Reward}
      ]),
      ok = pivot_client:do(rewards, set, PReq),
      reply(Reward, Req4);
    _ ->
      ?ERROR(<<"Invalid reward value">>, 400, Req)
  end;
check_method(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.

reply(Reward, Req) when is_float(Reward) ->
  reply(float_to_binary(Reward), Req);
reply(Reward, Req) ->
  cowboy_req:reply(200, [
    {<<"cache-control">>, <<"max-age=3600">>}
  ], Reward, Req).
