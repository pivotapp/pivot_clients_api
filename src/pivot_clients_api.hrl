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
  user,
  event,
  bandit,
  arm,
  arms,
  reward,
  selections,
  explore = true
}).

-define(SUPER_BANDIT, <<"$$SUPER_BANDIT$$">>).
