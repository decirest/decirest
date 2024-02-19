-module(decirest_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 1, 5}, []}}.
init([]) ->
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
