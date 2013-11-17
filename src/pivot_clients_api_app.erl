%% @private
-module(pivot_clients_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-include("pivot_clients_api.hrl").

%% API.

start(_Type, _Args) ->
  [begin
    ok = riakou:start_link(Group, URL)
  end || {Group, URL} <- ?RIAKOU_GROUPS],
  rate_limit:start(),
  pivot_clients_api_event_set:init(),

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
