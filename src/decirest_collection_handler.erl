-module(decirest_collection_handler).
-export([
  init/2,
  is_authorized/2,
  forbidden/2,
  content_types_provided/2,
  get_html/2,
  resource_exists/2
]).

init(Req, State) ->
  {cowboy_rest, Req, State#{rstate => #{}}}.

is_authorized(Req, State) ->
  decirest_auth:is_authorized(Req, State).

forbidden(Req, State) ->
  decirest_auth:forbidden(Req, State).

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State = #{child_fun := ChildFun, module := Module}) ->
  Children = ChildFun(Module),
  Data0 = case Module:fetch_data(cowboy_req:bindings(Req), State) of
            {ok, D} ->
              D;
            {error, _Msg} ->
              %lager:error("got exception when fetching data ~p", [Msg]),
              []
          end,
  PK = case erlang:function_exported(Module, data_pk, 0) of
         true ->
           Module:data_pk();
         false ->
           id
       end,
  Data = [data_prep(D, PKVal, Children, Req, State) || D = #{PK := PKVal} <- Data0],
  {<<"<html><body>", (jsx:encode(Data, [indent]))/binary, "</body></html>">>, Req, State}.

data_prep(Data, PK, Children, Req0 = #{path := Path}, State) ->
  Req = Req0#{path => decirest:pretty_path([Path, "/", integer_to_binary(PK)])},
  ChildUrls = decirest:child_urls_map(Children, Req, State),
  maps:merge(ChildUrls, Data);
data_prep(D, _, _, Req, _) ->
  io:format("prep failure, ~p~n", [Req]),
  D.

resource_exists(Req, State = #{mro_call := true}) ->
  {true, Req, State};
resource_exists(Req, State = #{module := Module}) ->
  Continue = fun({true, _, _}) -> true;(_) -> false end,
  Log = {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true, Continue),
  io:format("end resource_exists = ~p~n", [Log]),
  {maps:get(Module, Res, false), ReqNew, StateNew}.
