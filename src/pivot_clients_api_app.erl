%% @private
-module(pivot_clients_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  ok = riakou:start_link(simple_env:get_binary("RIAK_URL", <<"riak://localhost">>)),
  ok = riakou:wait_for_connection(),

  erlenv:configure(fun configure/1),

  {ok, _} = cowboy:start_http(http, 100, [
    {port, Port = simple_env:get_integer("PORT", 5000)}
  ], [
    {compress, true},
    {env, [
      {dispatch, cowboy_route_loader:compile(pivot_clients_api)}
    ]},
    {middlewares, [
      cowboy_env,
      {cowboy_fun, cowboy_request_id:init([
        {header, simple_env:get_binary("REQUEST_ID_HEADER", <<"x-request-id">>)}
      ])},
      cowboy_router,
      cowboy_handler
    ]}
  ]),

  io:format("Server started on port ~p\n", [Port]),
  simple_sup:start_link([]).

stop(_State) ->
  ok.

configure("development") ->
  sync:go();
configure(_)->
  ok.
