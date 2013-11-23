-define(ERROR (Error, Req), ?ERROR(Error, 500, Req)).
-define(ERROR (Error, Code, Req), cowboy_req:reply(Code, [
    {<<"content-type">>, <<"application/json">>},
    {<<"request-id">>, cowboy_request_id:get(Req)}
  ],
  jsx:encode([
    {<<"error">>, [
      {<<"message">>, Error},
      {<<"code">>, Code}
    ]}
  ]),
  Req)).

-define(TRANSPARENT_GIF, <<71,73,70,56,57,97,1,0,1,0,128,0,0,0,0,0,255,255,255,33,249,4,1,0,0,0,0,44,0,0,0,0,1,0,1,0,0,2,1,68,0,59>>).
