-module(decirest_collection_handler).
-export([
  init/2,
  is_authorized/2, is_authorized_default/2,
  forbidden/2, forbidden_default/2,
  allowed_methods/2, allowed_methods_default/2,
  content_types_accepted/2, content_types_accepted_default/2,
  from_fun/2, from_fun_default/2,
  content_types_provided/2,
  to_fun/2, to_fun_default/2,
  to_html/2, to_html_default/2,
  to_json/2, to_json_default/2,
  resource_exists/2, resource_exists_default/2
]).

init(Req, State) ->
  lager:info("collection init ~p", [Req]),
  {cowboy_rest, Req, State#{rstate => #{}}}.

is_authorized(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, is_authorized, Req, State, fun is_authorized_default/2).

is_authorized_default(Req, State) ->
  decirest_auth:is_authorized(Req, State).

forbidden(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, forbidden, Req, State, fun forbidden_default/2).

forbidden_default(Req, State = #{mro_call := true}) ->
  decirest_auth:forbidden(Req, State);
forbidden_default(Req, State = #{module := Module}) ->
  Continue = fun({false, _, _}) -> true; (_) -> false end,
  Log = {Res, ReqNew, StateNew} = decirest:call_mro(forbidden, Req, State, false, Continue),
  lager:debug("end forbidden = ~p~n", [Log]),
  {maps:get(Module, Res, true), ReqNew, StateNew}.

allowed_methods(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, allowed_methods, Req, State, fun allowed_methods_default/2).

allowed_methods_default(Req, State = #{module := Module}) ->
  Methods = case erlang:function_exported(Module, validate_payload, 3) or
    erlang:function_exported(Module, validate_payload, 2) of
              true->
                [<<"POST">>];
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

from_fun_default(Req0 = #{path := Path}, State = #{module := Module}) ->
  % gate 2 here
  {ok, Body, Req} = cowboy_req:read_body(Req0),
  PK = decirest:module_pk(Module),
  case validate_payload(Body, Req, State) of
    {ok, Payload = #{PK := ID}} ->
      % gate3 auth here
      case Module:persist_data(Payload, State) of
        {ok, NewState} ->
          SelfUrl = decirest:pretty_path([Path, "/", decirest:t2b(ID)]),
          {{true, SelfUrl}, Req, NewState};
        {error, State} ->
          ReqNew = cowboy_req:set_resp_body(<<"error">>, Req),
          {stop, ReqNew, State};
        {StatusCode, State} when is_number(StatusCode) ->
          ReqNew = cowboy_req:reply(StatusCode, Req),
          {stop, ReqNew, State};
        {StatusCode, Body, State} when is_number(StatusCode) ->
          ReqNew = cowboy_req:reply(StatusCode, #{}, Body, Req),
          {stop, ReqNew, State}
      end;
    {error, Errors} ->
      lager:critical("errors ~p", [Errors]),
      RespBody = jsx:encode(Errors),
      ReqNew = cowboy_req:set_resp_body(RespBody, Req),
      {false, ReqNew, State}
  end.

validate_payload(Body, Req, State = #{module := Module}) ->
  case erlang:function_exported(Module, validate_payload, 3) of
    true ->
      Module:validate_payload(Body, Req, State);
    false ->
      Module:validate_payload(Body, State)
  end.
content_types_provided(Req, State = #{module := Module}) ->
  Default = [
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"javascript">>, '*'}, to_json},
    {{<<"text">>, <<"html">>, '*'}, to_html},
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
  PK = decirest:module_pk(Module),
  Context = [
    {data_pk, PK},
    {title, Title},
    {collection_data, Json}
  ],
  {ok, Body} = std_response_html_dtl:render(Context),
  {Body, ReqNew, StateNew}.

to_json(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_json, Req, State, fun to_json_default/2).

to_json_default(Req, State = #{child_fun := ChildFun, module := Module, rstate := RState}) ->
  Children = ChildFun(Module),
  Data0 = case Module:fetch_data(cowboy_req:bindings(Req), RState) of
            {ok, D} ->
              D;
            {error, Msg} ->
              lager:error("got exception when fetching data ~p", [Msg]),
              []
          end,
  PK = case erlang:function_exported(Module, data_pk, 0) of
         true ->
           Module:data_pk();
         false ->
           id
       end,
  Data = [data_prep(D, PKVal, Children, Req, State) || D = #{PK := PKVal} <- Data0],
  {jsx:encode(Data, [indent]), Req, State}.

data_prep(Data, PK, Children, Req0 = #{path := Path}, State) ->
  SelfUrl = decirest:pretty_path([Path, "/", decirest:t2b(PK)]),
  Req = Req0#{path => SelfUrl},
  ChildUrls = decirest:child_urls_map(Children, Req, State),
  maps:merge(ChildUrls, Data#{details_url => SelfUrl});
data_prep(D, _, _, Req, _) ->
  lager:error("prep failure, ~p", [Req]),
  D.

resource_exists(Req, State = #{module := Module}) ->
decirest:apply_with_default(Module, resource_exists, [Req, State], fun resource_exists_default/2).

resource_exists_default(Req, State = #{mro_call := true}) ->
  {true, Req, State};
resource_exists_default(Req, State = #{module := Module}) ->
  Continue = fun({true, _, _}) -> true;(_) -> false end,
  Log = {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true, Continue),
  lager:debug("end resource_exists = ~p", [Log]),
  {maps:get(Module, Res, false), ReqNew, StateNew}.
