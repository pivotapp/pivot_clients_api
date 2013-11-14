%% -*- coding: utf-8 -*-
-module(pivot_client).

-export([p/1]).
-export([do_p/1]).
-export([s/1]).
-export([do/3]).
-export([do_async/3]).
-export([new/1]).

-include("pivot_clients_api.hrl").

-define(LOG(NS, Fun, Time, App, ReqID), io:format("measure#~p.~p=~pµs app=~s request_id=~s~n", [NS, Fun, Time, App, ReqID])).

-undef(LOG).
-define(LOG(NS, Fun, Time, App, ReqID), noop).

p(Funs) ->
  rpc:pmap({?MODULE, do_p}, [], Funs).

do_p({NS, Fun, Req}) ->
  case catch ?MODULE:do(NS, Fun, Req) of
    {'EXIT', {undef, _}} ->
      {error, {missing, NS, Fun}};
    {'EXIT', Error} ->
      {error, Error};
    {error, no_connections} = E ->
      io:format("count#no_connections.~s.~s=1 app=~s request_id=~s~n", [NS, Fun, Req#pivot_req.app, Req#pivot_req.id]),
      E;
    Res ->
      Res
  end.

%% TODO make series dependencies
s(_Funs) ->
  ok.

new(Props) ->
  new(Props, #pivot_req{}).

new([], Req) ->
  Req;
new([{id, V}|Props], Req) ->
  new(Props, Req#pivot_req{id = V});
new([{env, V}|Props], Req) ->
  new(Props, Req#pivot_req{env = V});
new([{app, V}|Props], Req) ->
  new(Props, Req#pivot_req{app = V});
new([{version, V}|Props], Req) ->
  new(Props, Req#pivot_req{version = V});
new([{user, V}|Props], Req) ->
  new(Props, Req#pivot_req{user = V});
new([{event, V}|Props], Req) ->
  new(Props, Req#pivot_req{event = V});
new([{bandit, V}|Props], Req) ->
  new(Props, Req#pivot_req{bandit = V});
new([{arm, V}|Props], Req) ->
  new(Props, Req#pivot_req{arm = V});
new([{arms, V}|Props], Req) ->
  new(Props, Req#pivot_req{arms = V});
new([{reward, V}|Props], Req) ->
  new(Props, Req#pivot_req{reward = V});
new([{selections, V}|Props], Req) ->
  new(Props, Req#pivot_req{selections = V}).

do(NS, Fun, Req = #pivot_req{id = _ReqID, app = _App}) ->
  {_Time, Res} = timer:tc(mod(NS), Fun, [Req]),
  ?LOG(NS, Fun, _Time, _App, _ReqID),
  Res.

do_async(NS, Fun, Req) ->
  %% TODO supervise this
  %% TODO if we've got a lot of backpressure let's block
  %% TODO allow for a priority setting based on {NS, Fun}
  spawn_link(?MODULE, do, [NS, Fun, Req]),
  ok.

mod(rewards) ->
  pivot_clients_api_rewards;
mod(events) ->
  pivot_clients_api_events;
mod(assignments) ->
  pivot_clients_api_assignments;
mod(arm_state) ->
  pivot_clients_api_arm_state;
mod(bandit_state) ->
  pivot_clients_api_bandit_state;
mod(selections) ->
  pivot_clients_api_selections;
mod(bandits) ->
  pivot_clients_api_bandits;
mod(arms) ->
  pivot_clients_api_arms;
mod(Mod) ->
  Mod.
