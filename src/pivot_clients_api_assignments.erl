%%
%% pivot_clients_api_assignments.erl
%%
-module(pivot_clients_api_assignments).

-export([get/1]).
-export([set/1]).
-export([assign/1]).

-include("pivot_clients_api.hrl").

-define(ASSIGNMENTS_BUCKET(Env), <<"assignments:", Env/binary>>).
-define(ASSIGNMENTS_KEY(App, UserID, Version), <<App/binary, ":", UserID/binary, ":", Version/binary>>).

get(#pivot_req{env = Env, app = App, version = Version, user = UserID}) ->
  case riakou:do(fetch_type, [{<<"map">>, ?ASSIGNMENTS_BUCKET(Env)}, ?ASSIGNMENTS_KEY(App, UserID, Version)]) of
    {ok, Obj} ->
      Assignments = [{Bandit, Arm} || {{Bandit, _}, Arm} <- riakc_map:value(Obj)],
      {ok, Assignments};
    {error, {notfound, _}} ->
      {ok, []}
  end.

assign(Req) ->
  %% TODO if the assignments call returns before the selections with valid assignments use those
  Res = pivot_client:p([
    {assignments, get, Req},
    {selections, get, Req}
  ]),
  handle_assignment(Res, Req).

handle_assignment([{ok, []}, {ok, Selections}], Req) ->
  ok = pivot_client:do_async(assignments, set, Req#pivot_req{selections = Selections}),
  {ok, filter_superbandit(Selections)};
handle_assignment([{ok, Assignments}, _], _) ->
  {ok, filter_superbandit(Assignments)};
handle_assignment([{ok, []}, Error], _) ->
  Error;
handle_assignment([Error, _], _) ->
  Error.

filter_superbandit(Assignments) ->
  [BA || BA = {Bandit, _} <- Assignments, Bandit =/= ?SUPER_BANDIT].

set(#pivot_req{env = Env, app = App, version = Version, user = UserID, selections = Selections}) ->
  Fun = fun(Map) ->
    update(remove(Map, Selections), Selections)
  end,
  BucketAndType = {<<"map">>, ?ASSIGNMENTS_BUCKET(Env)},
  Key = ?ASSIGNMENTS_KEY(App, UserID, Version),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],
  riakou:do(modify_type, Args).

update(Map, []) ->
  Map;
update(Map, [{Bandit, Arm}|Selections]) ->
  Map2 = riakc_map:update({Bandit, register}, fun(Reg) ->
    riakc_register:set(Arm, Reg)
  end, Map),
  update(Map2, Selections).

remove(Map, Selections) ->
  remove(Map, Selections, riakc_map:value(Map)).

remove(Map, _, []) ->
  Map;
remove(Map, Selections, [{{Bandit, register}, _}|Prev]) ->
  Map2 = case lists:keysearch(Bandit, 1, Selections) of
    false ->
      riakc_map:erase({Bandit, register}, Map);
    _ ->
      Map
  end,
  remove(Map2, Selections, Prev).
