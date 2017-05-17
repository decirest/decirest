-module(decirest_single_handler).
-export([
  init/2,
  content_types_provided/2,
  get_html/2,
  resource_exists/2
]).

init(Req, State) ->
  io:format("~p~n", [Req]),
  {cowboy_rest, Req, State#{rstate => #{}}}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State = #{child_fun := ChildFun, module := Module, rstate := RState}) ->
  #{Module := #{data := Data}} = RState,
  ChildUrls = decirest:child_urls_map(ChildFun(Module), Req, State),
  {<<"<html><body>", (jsx:encode(maps:merge(ChildUrls, Data), [indent]))/binary, "</body></html>">>, Req, State}.

resource_exists(Req, State = #{mro_call := true, module := Module, rstate := RState}) ->
  io:format("in resource exist single state ~p~n", [State]),
  case Module:fetch_data(cowboy_req:bindings(Req), RState) of
    {ok, [Data]} ->
      decirest_auth:gate2(Req, State#{rstate => RState#{Module => #{data => Data}}});
    {ok, Data} when is_list(Data) ->
      ReqNew = cowboy_req:reply(409, Req),
      {stop, ReqNew, State};
    {ok, Data} ->
      decirest_auth:gate2(Req, State#{rstate => RState#{Module => #{data => Data}}});
    {error, _Reason} ->
      {false, Req, State}
  end;
resource_exists(Req, State = #{module := Module}) ->
  Continue = fun({true, _, _}) -> true;(_) -> false end,
  Log = {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true, Continue),
  io:format("end resource_exists = ~p~n", [Log]),
  {maps:get(Module, Res, false), ReqNew, StateNew}.


