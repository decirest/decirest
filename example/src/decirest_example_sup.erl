-module(decirest_example_sup).
-behavior(supervisor).

-export([
  start_link/0,
  init/1
]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [],
  {ok, {{one_for_one, 1, 5}, Procs}}.
