-module(decirest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start(_Type, _Args) ->
	decirest_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
	ok.
