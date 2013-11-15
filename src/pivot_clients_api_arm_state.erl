%% -*- coding: utf-8 -*-
%%
%% pivot_clients_api_arm_state.erl
%%
-module(pivot_clients_api_arm_state).

-export([get/1]).
-export([add/1]).

-include("pivot_clients_api.hrl").

-define(STATE_BUCKET(Env), <<"state:", Env/binary>>).
-define(STATE_KEY(App, Version, Bandit, Arm), ?KEY_HASH(App, Version, Bandit, Arm)).

-define(BUCKET_SIZE, 50).
-define(BUCKET_COUNT, 20).

get(Req = #pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm}) ->
  case riakou:do(fetch_type, [{<<"map">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, Bandit, Arm)]) of
    {ok, Obj} ->
      Count = binary_to_integer(map_get({<<"n">>, register}, Obj, <<"0">>)),
      Score = binary_to_float(map_get({<<"s">>, register}, Obj, <<"0.0">>)),
      EventSet = map_get({<<"e">>, set}, Obj, []),
      %% NOTE
      %% This is not sorting because the riakc_set is an ordset. This is an implementation
      %% detail and should be considered when updating the client library.
      case compute_score(Count, Score, EventSet, Req) of
        {ok, ArmState} ->
          {ok, Arm, ArmState};
        Error ->
          Error
      end;
    {error, {notfound, _}} ->
      {ok, Arm, {0, 0.0}};
    Error ->
      Error
  end.

compute_score(Count, Score, [], _Req) ->
  {ok, {Count, Score}};
compute_score(Count, Score, [Event|EventSet], Req) ->
  case pivot_client:do(event_set, get, Req#pivot_req{event_set = Event, count = Count, score = Score}) of
    {ok, {NewCount, NewScore}} ->
      compute_score(NewCount, NewScore, EventSet, Req);
    %% We'll get this when someone cleaned up an event set but we didn't know about it
    {error, notfound} ->
      compute_score(Count, Score, EventSet, Req);
    Error ->
      Error
  end.

add(Req = #pivot_req{env = Env, app = App, version = Version, bandit = Bandit, arm = Arm}) ->
  Bucket = {<<"map">>, ?STATE_BUCKET(Env)},
  Key = ?STATE_KEY(App, Version, Bandit, Arm),
  case get_or_create(Bucket, Key) of
    {ok, Obj} ->
      {Current, Obj2} = current(Obj),
      EReq = Req#pivot_req{event_set = Current},

      case pivot_client:do(event_set, add, EReq) of
        {ok, Size} when Size > ?BUCKET_SIZE ->
          cleanup(Bucket, Key, Obj, EReq);
        {ok, _} ->
          maybe_update(Bucket, Key, Obj, Obj2);
        Error ->
          Error
      end;
    Error ->
      Error
  end.

maybe_update(_, _, Obj, Obj) ->
  ok;
maybe_update(Bucket, Key, _, Obj) ->
  riakou:do(update_type, [Bucket, Key, riakc_map:to_op(Obj), [create]]).

current(Obj) ->
  case map_get({<<"c">>, register}, Obj) of
    undefined ->
      C = init_bin(),
      {C, add_event_set(C, Obj)};
    C ->
      {C, Obj}
  end.

cleanup(Bucket, Key, Obj, Req = #pivot_req{event_set = Current}) ->
  NewCurrent = inc_bin(Current),
  Obj2 = add_event_set(NewCurrent, Obj),
  EventSet = map_get({<<"e">>, set}, Obj2),
  Length = length(EventSet),

  {ok, Obj3, SetsToDelete} = case Length > ?BUCKET_COUNT of
    true ->
      Count = binary_to_integer(map_get({<<"n">>, register}, Obj, <<"0">>)),
      Score = binary_to_float(map_get({<<"s">>, register}, Obj, <<"0.0">>)),
      %% NOTE
      %% This is not sorting because the riakc_set is an ordset. This is an implementation
      %% detail and should be considered when updating the client library.
      case compute_score_and_cleanup(Count, Score, EventSet, Req, Obj2, Length, []) of
        {ok, _, _} = Res ->
          Res;
        _ ->
          %% We're just going to ignore errors and have someone else try to clean up
          {ok, Obj2, []}
      end;
    _ ->
      {ok, Obj2, []}
  end,
  case riakou:do(update_type, [Bucket, Key, riakc_map:to_op(Obj3), [create]]) of
    ok ->
      %% TODO make this more resilient to errors
      [pivot_client:do(event_set, delete, Req#pivot_req{event_set = E}) || E <- SetsToDelete],
      ok;
    Error ->
      Error
  end.

compute_score_and_cleanup(Count, Score, _, _, Obj, ?BUCKET_COUNT, SetsToDelete) ->
  NewObj = riakc_map:update({<<"n">>, register}, fun(Reg) ->
    riakc_register:set(integer_to_binary(Count), Reg)
  end, Obj),
  {ok, riakc_map:update({<<"s">>, register}, fun(Reg) ->
    riakc_register:set(float_to_binary(Score, [{decimals, 10}, compact]), Reg)
  end, NewObj), SetsToDelete};
compute_score_and_cleanup(Count, Score, [Event|EventSet], Req, Obj, Remaining, SetsToDelete) ->
  EReq = Req#pivot_req{event_set = Event},
  case pivot_client:do(event_set, get, EReq#pivot_req{count = Count, score = Score}) of
    {ok, {NewCount, NewScore}} ->
      NewObj = riakc_map:update({<<"e">>, set}, fun(Set) ->
        riakc_set:del_element(Event, Set)
      end, Obj),
      compute_score_and_cleanup(NewCount, NewScore, EventSet, Req, NewObj, Remaining - 1, [Event|SetsToDelete]);
    Error ->
      Error
  end.

add_event_set(Current, Obj) ->
  NewObj = riakc_map:update({<<"c">>, register}, fun(Reg) ->
    riakc_register:set(Current, Reg)
  end, Obj),
  riakc_map:update({<<"e">>, set}, fun(Set) ->
    riakc_set:add_element(Current, Set)
  end, NewObj).

get_or_create(Bucket, Key) ->
  case riakou:do(fetch_type, [Bucket, Key]) of
    {ok, Obj} ->
      {ok, Obj};
    {error, {notfound, _}} ->
      {ok, riakc_map:new()};
    Error ->
      Error
  end.

map_get(Key, Obj) ->
  map_get(Key, Obj, undefined).
map_get(Key, Obj, Default) ->
  case riakc_map:find(Key, Obj) of
    error ->
      Default;
    {ok, Val} ->
      Val
  end.

init_bin() ->
  inc_bin(<<0>>).

inc_bin(Bin) ->
  <<(binary:decode_unsigned(Bin) + 1):32>>.
