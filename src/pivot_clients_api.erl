%%
%% pivot_clients_api.erl
%%
-module(pivot_clients_api).

-export([start/0]).

%% API.

start() ->
  ok = inets:start(),
  ok = riakou:start(),
  application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(pivot_clients_api).
