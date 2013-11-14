%% -*- coding: utf-8 -*-
%%
%% pivot_clients_api_arm_state.erl
%%
-module(pivot_clients_api_arm_state).

-export([get/1]).
-export([add/1]).

-include("pivot_clients_api.hrl").

-define(STATE_BUCKET(Env), <<"state:", Env/binary>>).
-define(STATE_KEY(App, Version, Bandit, Arm), <<App/binary, ":", Version/binary, ":", Bandit/binary, ":", Arm/binary>>).

-define(BUCKET_SIZE, 100).
-define(BUCKET_COUNT, 10).

get(#pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm}) ->
  case riakou:do(fetch_type, [{<<"map">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, Bandit, Arm)]) of
    {ok, Obj} ->
      State = riakc_map:value(Obj),
      Count = fast_key:get({<<"n">>, register}, State, 0),
      Score = fast_key:get({<<"a">>, register}, State, 0.0),
      Stage = fast_key:get({<<"s">>, map}, State, []),
      {ok, Arm, compute_score(Count, Score, lists:keysort(1, Stage))};
    {error, {notfound, _}} ->
      {ok, Arm, {0, 0.0}};
    Error ->
      Error
  end.

add(Req = #pivot_req{env = Env, app = App, version = Version, user = UserID, bandit = Bandit, arm = Arm, reward = Reward}) ->
  Fun = fun(ArmMap) ->
    %% find the current 'bucket'
    {ArmBucket, ArmMap2} = current(ArmMap),
    riakc_map:update({<<"s">>, map}, fun(StageMap) ->
      riakc_map:update({ArmBucket, set}, fun(StageBucket) ->
        riakc_set:add_element(term_to_binary({now(), UserID, Reward}), StageBucket)
      end, StageMap)
    end, ArmMap2)
  end,
  BucketAndType = {<<"map">>, ?STATE_BUCKET(Env)},
  Key = ?STATE_KEY(App, Version, Bandit, Arm),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],

  case riakou:do(modify_type, Args) of
    ok ->
      pivot_client:do_async(selections, renew, Req);
    Error ->
      Error
  end.

current(ArmMap) ->
  case riakc_map:find({<<"c">>, register}, ArmMap) of
    {ok, CurrentArmBucket} ->
      SelectedArmBucket = case riakc_map:find({<<"s">>, map}, ArmMap) of
        {ok, StagingMap} ->
          case fast_key:get({CurrentArmBucket, set}, StagingMap) of
            undefined ->
              CurrentArmBucket;
            StageBucket ->
              case length(StageBucket) of
                Size when Size > ?BUCKET_SIZE ->
                  <<Bit>> = CurrentArmBucket,
                  <<(Bit + 1)>>;
                _ ->
                  CurrentArmBucket
              end
          end;
        _ ->
          CurrentArmBucket
      end,
      case SelectedArmBucket of
        CurrentArmBucket ->
          {SelectedArmBucket, ArmMap};
        _ ->
          NewArmMap = riakc_map:update({<<"c">>, register}, fun(Reg) ->
            riakc_register:set(SelectedArmBucket, Reg)
          end, ArmMap),
          {SelectedArmBucket, NewArmMap}
      end;
    _ ->
      NewArmBucket = <<0>>,
      NewArmMap = riakc_map:update({<<"c">>, register}, fun(Reg) ->
        riakc_register:set(NewArmBucket, Reg)
      end, ArmMap),
      {NewArmBucket, NewArmMap}
  end.

compute_score(Count, Score, []) ->
  {Count, Score};
compute_score(Count, Score, [{_, Events}|Stage]) ->
  SortedEvents = lists:keysort(1, [binary_to_term(Event) || Event <- Events]),
  {NewCount, NewScore} = chain(Count, Score, SortedEvents),
  compute_score(NewCount, NewScore, Stage).

chain(Count, Score, []) ->
  {Count, Score};
chain(Count, Score, [{T, U, Reward}|Events]) when is_binary(Reward) ->
  chain(Count, Score, [{T, U, binary_to_float(Reward)}|Events]);
chain(Count, Score, [{_, _, Reward}|Events]) ->
  N = Count + 1,
  NewScore = ((N - 1.0) / N) * Score + (1.0 / N) * Reward,
  chain(N, NewScore, Events).
