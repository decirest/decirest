-module(decirest_collection_handler).
-export([
  init/2,
  content_types_provided/2,
  get_html/2
]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
  {<<"<html><body>", (jsx:encode(State#{child_fun => null}, [indent]))/binary, "</body></html>">>, Req, State}.
