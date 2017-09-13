-module(decirest_single_handler).
-export([
  init/2,
  is_authorized/2, is_authorized_default/2,
  forbidden/2, forbidden_default/2,
  allowed_methods/2, allowed_methods_default/2,
  content_types_accepted/2, content_types_accepted_default/2,
  from_fun/2, from_fun_default/2,
  content_types_provided/2, content_types_provided_default/2,
  to_fun/2, to_fun_default/2,
  to_html/2, to_html_default/2,
  to_json/2, to_json_default/2,
  resource_exists/2, resource_exists_default/2
]).

init(Req, State) ->
  lager:info("single init ~p", [Req]),
  {cowboy_rest, Req, State#{rstate => #{}}}.

is_authorized(Req, State)
allowed_methods(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, allowed_methods, Req, State, fun allowed_methods_default/2).

allowed_methods_default(Req, State = #{module := Module}) ->
  Methods = case erlang:function_exported(Module, validate_payload, 2) of
              true ->
                [<<"PUT">>];
              false ->
                []
            end,
  {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">> | Methods], Req, State}.

content_types_accepted(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, content_types_accepted, Req, State, fun content_types_accepted_default/2).

content_types_accepted_default(Req, State) ->
  lager:critical("here I am"),
  {[
    {{<<"application">>, <<"json">>, []}, from_fun},
    {{<<"application">>, <<"javascript">>, []}, from_fun}
  ], Req, State}.

from_fun(Req, State = #{module := Module}) ->
  lager:critical("form fun single"),
  decirest:do_callback(Module, from_fun, Req, State, fun from_fun_default/2).

from_fun_default(Req0, State = #{module := Module}) ->
  % gate 2 here
  {ok, Body, Req} = cowboy_req:read_body(Req0),
  case Module:validate_payload(Body, State) of
    {ok, Payload} ->
      % gate3 auth here
      case Module:persist_data(Payload, State) of
        {ok, NewState} ->
          {true, Req, NewState};
        {error, State} ->
          ReqNew = cowboy_req:set_resp_body(<<"error">>, Req),
          {stop, ReqNew, State}
      end;
    {error, Errors} ->
      lager:critical("errors ~p", [Errors]),
      RespBody = jsx:encode(Errors),
      ReqNew = cowboy_req:set_resp_body(RespBody, Req),
      {false, ReqNew, State}
  end.

content_types_provided(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, content_types_provided, Req, State, fun content_types_provided_default/2).

content_types_provided_default(Req, State) ->
  {[
    {{<<"text">>, <<"html">>, '*'}, to_html},
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"javascript">>, '*'}, to_json},
    {{<<"application">>, <<"octet-stream">>, '*'}, to_fun}
  ], Req, State}.

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


