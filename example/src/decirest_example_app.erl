-module(decirest_example_app).
-behavior(application).

-export([
  start/2,
  stop/1,
  start_decirest/0
]).

start(_, _) ->
  decirest_example_db:init_dbs(),
  decirest_example_db:create_test_data(),
  start_decirest(),
  decirest_example_sup:start_link().

stop(_) ->
  ok.

start_decirest() ->
  Modules = [
    api_r,
    bo_r,
    account_r,
    project_r,
    user_r,
    financial_r
  ],

  lager:set_loglevel(lager_console_backend, debug),
  Rotes = decirest_router:build_routes(Modules),
  Dispatch = cowboy_router:compile(Rotes),
  {ok, _} = cowboy:start_clear(inapi, [{port, 8081}],
    #{env => #{dispatch => Dispatch}}
  ).
