-define(ERROR (Error, Req), ?ERROR(Error, 500, Req)).
-define(ERROR (Error, Code, Req), cowboy_req:reply(Code, [
    {<<"content-type">>, <<"application/json">>}
  ],
  [<<"{\"error\":{\"message\":\"">>,
      Error,
   <<"\",\"code\":">>,
    integer_to_binary(Code),
   <<"}}">>],
  Req)).

-record(pivot_req, {
  id,
  env,
  app,
  version = <<"*">>,
  token,
  event,
  bandit,
  arm,
  arms,
  reward,
  selections,
  explore = true,
  event_set,
  count,
  score
}).

-define(SUPER_BANDIT, <<"$$SUPER_BANDIT$$">>).

-define(KEYD, <<0>>).
-define(KEY_HASH, sha).
-define(KEY_HASH(BinToHash), (crypto:hash(?KEY_HASH, BinToHash))).
-define(KEY_HASH(A, B), ?KEY_HASH([A, ?KEYD, B])).
-define(KEY_HASH(A, B, C), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C])).
-define(KEY_HASH(A, B, C, D), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D])).
-define(KEY_HASH(A, B, C, D, E), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D, ?KEYD, E])).
-define(KEY_HASH(A, B, C, D, E, F), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D, ?KEYD, E, ?KEYD, F])).

%% riakou groups
-define(ARM_STATE_GROUP, pdefault).
-define(ARMS_GROUP, pdefault).
-define(ASSIGNMENTS_GROUP, assigments).
-define(BANDITS_GROUP, pdefault).
-define(EVENT_SET_GROUP, event_set).
-define(REWARDS_GROUP, pdefault).
-define(SELECTIONS_GROUP, pdefault).

-define(RIAKOU_GROUPS, [
  {?ARM_STATE_GROUP, <<"riak://localhost:8087">>},
  {?ASSIGNMENTS_GROUP, <<"riak://localhost:8086">>},
  {?EVENT_SET_GROUP, <<"riak://localhost:8088">>}
]).
