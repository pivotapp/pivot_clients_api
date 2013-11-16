%%
%% pivot_clients_api_events.erl
%%
-module(pivot_clients_api_events).

-export([add/1]).

-include("pivot_clients_api.hrl").

add(Req) ->
  Res = pivot_client:p([
    {rewards, get, Req},
    {assignments, get, Req}
  ]),
  reward(Res, Req).

reward([{ok, Reward}, {ok, Assignments}], Req) when length(Assignments) > 0 ->
  pivot_client:p([
    {arm_state, add, Req#pivot_req{bandit = Bandit, arm = Arm, reward = Reward}}
  || {Bandit, Arm} <- Assignments]);
  % pivot_client:do_async(selections, renew, Req);
reward({error, notfound}, _) ->
  noop;
reward(_, {error, notfound}) ->
  noop.
