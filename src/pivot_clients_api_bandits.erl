%%
%% pivot_clients_api_bandits.erl
%%
-module(pivot_clients_api_bandits).

-export([list/1]).
-export([enabled/1]).
-export([enable/1]).
-export([disabled/1]).
-export([disable/1]).
-export([remove/1]).
-export([clear/1]).

-export([get_map/1]).
-export([update_dt/2]).

-include("pivot_clients_api.hrl").

-define(BANDITS_BUCKET(Env), <<"bandits:", Env/binary>>).
-define(BANDITS_KEY(App), <<App/binary>>).

list(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [{Bandit, Enabled} || {{Bandit, flag}, Enabled} <- Bandits]};
    Error ->
      Error
  end.

enabled(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [Bandit || {{Bandit, flag}, true} <- Bandits]};
    Error ->
      Error
  end.

enable(Req) ->
  update(Req, enable).

disabled(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [Bandit || {{Bandit, flag}, false} <- Bandits]};
    Error ->
      Error
  end.

disable(Req) ->
  update(Req, disable).

remove(Req = #pivot_req{bandit = Bandit}) ->
  Fun = fun(Map) ->
    riakc_map:erase({Bandit, flag}, Map)
  end,
  update_dt(Fun, Req).

clear(Req = #pivot_req{env = Env, app = App}) ->
  case riakou:do(delete, [?BANDITS_BUCKET(Env), ?BANDITS_KEY(App)]) of
    ok ->
      pivot_client:do_async(selections, renew, Req);
    Error ->
      Error
  end.

update(Req = #pivot_req{bandit = Bandit}, Op) ->
  Fun = fun(Map) ->
    riakc_map:update({Bandit, flag}, fun(Flag) ->
      riakc_flag:Op(Flag)
    end, Map)
  end,
  update_dt(Fun, Req).

get_map(#pivot_req{env = Env, app = App}) ->
  case riakou:do(fetch_type, [{<<"map">>, ?BANDITS_BUCKET(Env)}, ?BANDITS_KEY(App)]) of
    {ok, Obj} ->
      {ok, riakc_map:value(Obj)};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

update_dt(Fun, Req = #pivot_req{env = Env, app = App}) ->
  BucketAndType = {<<"map">>, ?BANDITS_BUCKET(Env)},
  Key = ?BANDITS_KEY(App),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],

  case riakou:do(modify_type, Args) of
    ok ->
      pivot_client:do_async(selections, renew, Req);
    Error ->
      Error
  end.
