PROJECT = pivot_clients_api

# Dependencies

PKG_FILE_URL = https://gist.github.com/CamShaft/815c139ad3c1ccf13bad/raw/packages.tsv

DEPS = cowboy fast_key simple_env cowboy_route_loader simple_sup sync erlenv cowboy_env cowboy_request_id cowboy_fun jsx riakc riakou privdir websaferl cowboy_cors

dep_cowboy = pkg://cowboy 0.8.6
dep_fast_key = pkg://fast_key master
dep_simple_env = pkg://simple_env master
dep_cowboy_route_loader = pkg://cowboy_route_loader master
dep_simple_sup = pkg://simple_sup master
dep_sync = pkg://sync master
dep_erlenv = pkg://erlenv master
dep_cowboy_env = pkg://cowboy_env master
dep_cowboy_request_id = pkg://cowboy_request_id master
dep_cowboy_fun = pkg://cowboy_fun master
dep_jsx = pkg://jsx v1.4.3
dep_riakc = pkg://riakc master
dep_riakou = pkg://riakou master

dep_privdir = https://github.com/CamShaft/privdir.git master
dep_websaferl = https://github.com/CamShaft/websaferl.git master
dep_cowboy_cors = https://github.com/CamShaft/cowboy_cors.git master

include erlang.mk
