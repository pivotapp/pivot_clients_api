%%
%% pivot_clients_api_selections.erl
%%
-module(pivot_clients_api_selections).

-export([get/1]).
-export([set/1]).
-export([renew/1]).

-include("pivot_clients_api.hrl").

-define(SELECTIONS_BUCKET(Env), <<"selections:", Env/binary>>).
-define(SELECTIONS_KEY(App, Version), ?KEY_HASH(App, Version)).

get(#pivot_req{env = Env, app = App, version = Version}) ->
  case riakou:do(fetch_type, [{<<"map">>, ?SELECTIONS_BUCKET(Env)}, ?SELECTIONS_KEY(App, Version)]) of
    {ok, Obj} ->
      {ok, [{Bandit, Arm} || {{Bandit, _}, Arm} <- riakc_map:value(Obj)]};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

renew(Req) ->
  case pivot_client:do(bandits, enabled, Req#pivot_req{bandit = undefined}) of
    {ok, []} ->
      ok;
    {ok, Bandits} ->
      {ok, _, ExploreBandit} = pivot_client:do(bandit_state, select, Req#pivot_req{bandit = ?SUPER_BANDIT, arms = Bandits}),
      Selections = pivot_client:p([
        {bandit_state, select, Req#pivot_req{bandit = Bandit, explore = (ExploreBandit =:= Bandit)}}
      || Bandit <- Bandits]),
      Filtered = [{Bandit, Arm} || {ok, Bandit, Arm} <- Selections],
      pivot_client:do(selections, set, Req#pivot_req{selections = [{?SUPER_BANDIT, ExploreBandit}|Filtered]});
    Error ->
      Error
  end.

set(#pivot_req{env = Env, app = App, version = Version, selections = Selections}) ->
  Fun = fun(Map) ->
    update(remove(Map, Selections), Selections)
  end,
  BucketAndType = {<<"map">>, ?SELECTIONS_BUCKET(Env)},
  Key = ?SELECTIONS_KEY(App, Version),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],
  riakou:do(modify_type, Args).

update(Map, []) ->
  Map;
update(Map, [{_, undefined}|Selections]) ->
  update(Map, Selections);
update(Map, [{undefined, _}|Selections]) ->
  update(Map, Selections);
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
