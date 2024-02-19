-module(decirest_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_, _) -> {error, _} | {ok, pid()}.
start(_Type, _Args) ->
    Handlers =
        [decirest_single_handler,
         decirest_collection_handler,
         decirest_ws_handler,
         decirest_doc_resource],
    [Handler:module_info() || Handler <- Handlers],
    decirest_sup:start_link().

-spec stop(_) -> ok.
stop(_State) -> ok.
