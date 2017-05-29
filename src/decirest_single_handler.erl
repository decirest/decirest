-module(decirest_single_handler).
-export([
  init/2,
  content_types_provided/2,
  to_fun/2,
  to_html/2,
  to_json/2,
  resource_exists/2
]).

init(Req, State) ->
  lager:info("single init ~p", [Req]),
  {cowboy_rest, Req, State#{rstate => #{}}}.

content_types_provided(Req, State = #{module := Module}) ->
  Default = [
    {{<<"text">>, <<"html">>, '*'}, to_html},
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"javascript">>, '*'}, to_json},
    {{<<"application">>, <<"octet-stream">>, '*'}, to_fun}
  ],
  decirest:do_callback(Module, content_types_provided,Req, State, Default).

to_fun(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_fun, Req, State, fun to_fun_default/2).

to_fun_default(Req, State) ->
  to_json(Req, State).

to_html(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_html, Req, State, fun to_html_default/2).

to_html_default(Req, State = #{module := Module}) ->
  {Json, ReqNew, StateNew} = to_json(Req, State),
  Title = Module:name(),
  Context = [
    {title, Title},
    {single_data, Json}
  ],
  {ok, Body} = std_response_html_dtl:render(Context),
  {Body, ReqNew, StateNew}.

to_json(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_json, Req, State, fun to_json_default/2).

to_json_default(Req, State = #{child_fun := ChildFun, module := Module, rstate := RState}) ->
  #{Module := #{data := Data}} = RState,
  ChildUrls = decirest:child_urls_map(ChildFun(Module), Req, State),
  {jsx:encode(maps:merge(ChildUrls, Data), [indent]), Req, State}.

resource_exists(Req, State = #{module := Module}) ->
  decirest:apply_with_default(Module, resource_exists, [Req, State], fun resource_exists_default/2).

resource_exists_default(Req, State = #{mro_call := true, module := Module, rstate := RState}) ->
  lager:debug("in resource exist single state ~p~n", [State]),
  case Module:fetch_data(cowboy_req:bindings(Req), RState) of
    {ok, [Data]} ->
      decirest_auth:gate2(Req, State#{rstate => RState#{Module => #{data => Data}}});
    {ok, []} ->
      {false, Req, State};
    {ok, Data} when is_list(Data) ->
      ReqNew = cowboy_req:reply(409, Req),
      {stop, ReqNew, State};
    {ok, Data} ->
      decirest_auth:gate2(Req, State#{rstate => RState#{Module => #{data => Data}}});
    {error, _Reason} ->
      {false, Req, State}
  end;
resource_exists_default(Req, State = #{module := Module}) ->
  Continue = fun({true, _, _}) -> true;(_) -> false end,
  Log = {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true, Continue),
  lager:debug("end resource_exists = ~p~n", [Log]),
  {maps:get(Module, Res, false), ReqNew, StateNew}.


