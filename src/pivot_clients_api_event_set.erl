%%
%% pivot_clients_api_event_set.erl
%%
-module(pivot_clients_api_event_set).

-export([get/1]).
-export([add/1]).
-export([delete/1]).
-export([encode/2]).
-export([decode/1]).

-include("pivot_clients_api.hrl").

-define(STATE_BUCKET(Env), <<"eset:", Env/binary>>).
-define(STATE_KEY(App, Version, Bandit, Arm, EventSet), ?KEY_HASH(App, Version, Bandit, Arm, EventSet)).

add(#pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm, event_set = Set, reward = Reward, user = UserID}) ->
  Bucket = {<<"set">>, ?STATE_BUCKET(Env)},
  Key = ?STATE_KEY(App, Version, Bandit, Arm, Set),
  case get_or_create(Bucket, Key) of
    {ok, EventSet} ->
      NewEventSet = riakc_set:add_element(encode(UserID, Reward), EventSet),
      case riakou:do(update_type, [Bucket, Key, riakc_set:to_op(NewEventSet), [create]]) of
        ok ->
          {ok, riakc_set:size(NewEventSet)};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

get_or_create(Bucket, Key) ->
  case riakou:do(fetch_type, [Bucket, Key]) of
    {ok, EventSet} ->
      {ok, EventSet};
    {error, {notfound, _}} ->
      {ok, riakc_set:new()};
    Error ->
      Error
  end.

get(#pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm, event_set = Set, count = Count, score = Score}) ->
  case riakou:do(fetch_type, [{<<"set">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, Bandit, Arm, Set)]) of
    {ok, Obj} ->
      %% NOTE
      %% This is not sorting because the riakc_set is an ordset. This is an implementation
      %% detail and should be considered when updating the client library.
      EventsSet = riakc_set:value(Obj),
      {ok, chain(Count, Score, EventsSet)};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

delete(#pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm, event_set = Set}) ->
  riakou:do(delete, [{<<"set">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, Bandit, Arm, Set)]).

chain(Count, Score, []) ->
  {Count, Score};
chain(Count, Score, [Event|Events]) ->
  N = Count + 1,
  NewScore = ((N - 1.0) / N) * Score + (1.0 / N) * decode(Event),
  chain(N, NewScore, Events).

id(User) ->
  {Megasecs, Secs, Microsecs} = erlang:now(),
  Time = (((Megasecs * 1000000) + Secs) * 1000000) + Microsecs,
  Hash = erlang:phash2(User),
  <<Time:64, Hash:32>>.

encode(User, Reward) when is_float(Reward) ->
  encode(User, float_to_binary(Reward, [{decimals, 10}, compact]));
encode(User, Reward) ->
  <<(id(User))/binary, Reward/binary>>.

decode(<<_:96, Reward/binary>>) ->
  binary_to_float(Reward).
