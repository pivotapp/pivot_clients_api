%%
%% pivot_clients_api_events.erl
%%
-module(pivot_clients_api_events).

-export([add/1]).

-include("pivot_clients_api.hrl").

add(Req) ->
  case pivot_client:do(assignments, get, Req) of
    {ok, Assignments} ->
      reward(pivot_client:do(rewards, get, Req), Assignments, Req);
    _ ->
      noop
  end.

reward({ok, Reward}, Assignments, Req) when length(Assignments) > 0 ->
  pivot_client:p([
    {arm_state, add, Req#pivot_req{bandit = Bandit, arm = Arm, reward = Reward}}
  || {Bandit, Arm} <- Assignments]);
  % pivot_client:do_async(selections, renew, Req);
reward({error, notfound}, _, _) ->
  noop.
