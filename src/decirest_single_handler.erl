-module(decirest_single_handler).
-export([
  init/2,
  is_authorized/2,
  forbidden/2,
  allowed_methods/2, allowed_methods_default/2,
  options/2,
  content_types_accepted/2,
  content_types_provided/2, content_types_provided_default/2,
  from_fun/2, from_fun_default/2,
  from_multi/2,
  to_fun/2, to_fun_default/2,
  to_csv/2, to_csv_default/2,
  to_html/2, to_html_default/2,
  to_json/2, to_json_default/2,
  delete_resource/2, delete_resource_default/2,
  resource_exists/2, resource_exists_default/2
]).

-spec init(_,map()) -> {'cowboy_rest',_,#{'rstate':=#{}, _=>_}}.
init(Req, State) ->
  decirest_handler_lib:init_rest(Req, State).

-spec is_authorized(_,#{'module':=atom(), _=>_}) -> any().
is_authorized(Req, State) ->
  decirest_handler_lib:is_authorized(Req, State).

-spec forbidden(_,#{'module':=atom(), _=>_}) -> any().
forbidden(Req, State) ->
  decirest_handler_lib:forbidden(Req, State).

-spec allowed_methods(_,#{'module':=atom(), _=>_}) -> any().
allowed_methods(Req, State = #{module := Module}) ->
  {Methods, Req1, State1} =
    decirest:do_callback(Module, allowed_methods, Req, State, fun allowed_methods_default/2),
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      %% We need to keep allowed_methods in state to
      %% be able to return in headers if recource implement
      %% options/2 call back.
      {Methods, Req1, State1#{allowed_methods => Methods}};
    _ ->
      {Methods, Req1, State1}
  end.

-spec allowed_methods_default(_,#{'module':=atom(), _=>_}) -> {[<<_:24,_:_*8>>,...],_,#{'module':=atom(), _=>_}}.
allowed_methods_default(Req, State = #{module := Module}) ->

  DefaultMethods = [<<"HEAD">>, <<"GET">>, <<"OPTIONS">>],

  ExportMappingList =
    [
      {{persist_data,  [2, 3]}, [<<"PUT">>, <<"PATCH">>]},
      {{delete_data,   [2, 3]}, [<<"DELETE">> ]},
      {{action_schema, [0]},    [<<"POST">>]}
    ],

  Methods = decirest_handler_lib:export_to_methods(Module, ExportMappingList, DefaultMethods),

  {Methods, Req, State}.

options(Req, State) ->
  decirest_handler_lib:options(Req, State).

-spec content_types_accepted(_,#{'module':=atom(), _=>_}) -> any().
content_types_accepted(Req, State) ->
  decirest_handler_lib:content_types_accepted(Req, State).

from_multi(Req0, State0) ->
  {File, Req, State} = decirest_handler_lib:from_multi(Req0, State0),
  handle_body(File, Req, State).

-spec from_fun(_,#{'module':=atom(), _=>_}) -> any().
from_fun(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, from_fun, Req, State, fun from_fun_default/2).

-spec from_fun_default(map(),#{'module':=atom(), _=>_}) -> {'false',#{'resp_body':=_, _=>_},#{'module':=atom(), _=>_}} | {'stop',map(),#{'module':=atom(), _=>_}} | {'true',map(),_}.
from_fun_default(Req0, State) ->
  % gate 2 here
  {ok, Body, Req} = cowboy_req:read_body(Req0),
  handle_body(Body, Req, State).


handle_body(Body, Req = #{method := Method}, State = #{module := Module}) ->
  %% We need to check action_schema to not break legacy behavior
  %% eventuallu post on single should only be action
  case {Method, decirest_handler_lib:is_exported(Module, action_schema, 0)} of
    {<<"POST">>, true} ->
      validate_action(Body, Req, State);
    _ ->
      %% Legacy take all others for now
      validate_payload(Body, Req, State)
  end.

validate_action(Body, Req, State) ->
  case decirest_handler_lib:validate_action(Body, Req, State) of
    {ok, Payload} ->
      case decirest_handler_lib:perform_action(Payload, Req, State) of
        ok ->
          {true, Req, State};
        {error, Reason} ->
          {false, Req, State};
        {ok, ResBody} ->
          RespBody = jiffy:encode(ResBody, [force_utf8]),
          ReqNew = cowboy_req:set_resp_body(RespBody, Req),
          {true, ReqNew, State}
      end;
        {error, Error} ->
      decirest_handler_lib:return_error(Error, Req, State)
  end.

validate_payload(Body, Req = #{method := Method}, State) ->
  MB = get_module_binding(Req, State),
  case decirest_handler_lib:validate_payload(Body, Req, State#{method => Method, module_binding => MB}) of
    {ok, Payload} ->
      % gate3 auth here
      case decirest_handler_lib:persist_data(Payload, Req, State) of
        {ok, NewState} ->
          {true, Req, NewState};
        {error, NewState} ->
          ReqNew = cowboy_req:set_resp_body(<<"error">>, Req),
          {stop, ReqNew, NewState};
        {StatusCode, NewState} when is_number(StatusCode) ->
          ReqNew = cowboy_req:reply(StatusCode, Req),
          {stop, ReqNew, NewState};
        {StatusCode, RespBody, NewState} when is_number(StatusCode) ->
          ReqNew = cowboy_req:reply(StatusCode, #{}, RespBody, Req),
          {stop, ReqNew, NewState}
      end;
    {stop, NewReq, NewState} ->
      {stop, NewReq, NewState};
    {error, Errors} ->
      decirest_handler_lib:return_error(Errors, Req, State)
  end.

get_module_binding(Req, #{module := Module}) ->
  case decirest_handler_lib:is_exported(Module, ident, 0) of
    true ->
      cowboy_req:binding(Module:ident(), Req);
    false ->
      undefined
  end.

-spec delete_resource(map(), #{module := atom(), _ => _}) -> {true | false, map(), map()}.
delete_resource(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, delete_resource, Req, State, fun delete_resource_default/2).

-spec delete_resource_default(map(), #{module := atom(), _ => _}) -> {true | false, map(), map()}.
delete_resource_default(Req, State) ->
  decirest_handler_lib:delete_data(Req, State).

-spec content_types_provided(_,#{'module':=atom(), _=>_}) -> any().
content_types_provided(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, content_types_provided, Req, State, fun content_types_provided_default/2).

-spec content_types_provided_default(_,_) -> {[{{_,_,_},'to_fun' | 'to_html' | 'to_json'},...],_,_}.
content_types_provided_default(Req, State) ->
  {[
    {{<<"text">>, <<"html">>, '*'}, to_html},
    {{<<"text">>, <<"csv">>, '*'}, to_csv},
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"javascript">>, '*'}, to_json},
    {{<<"application">>, <<"octet-stream">>, '*'}, to_fun}
  ], Req, State}.

-spec to_fun(_,#{'module':=atom(), _=>_}) -> any().
to_fun(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_fun, Req, State, fun to_fun_default/2).

-spec to_fun_default(_,#{'module':=atom(), _=>_}) -> any().
to_fun_default(Req, State) ->
  to_json(Req, State).


-spec to_csv(_,#{'module':=atom(), _=>_}) -> any().
to_csv(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_csv, Req, State, fun to_csv_default/2).

-spec to_csv_default(_,#{'module':=atom(), _=>_}) -> {_,_,_}.
to_csv_default(Req, State = #{module := Module}) ->
  Data = decirest:get_data(Module, State),
  Title = Module:name(),
  Body = iolist_to_binary(decirest_handler_lib:to_csv(Title, Data)),
  {Body, Req, State}.

-spec to_html(_,#{'module':=atom(), _=>_}) -> any().
to_html(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_html, Req, State, fun to_html_default/2).

-spec to_html_default(_,#{'module':=atom(), _=>_}) -> {_,_,_}.
to_html_default(Req, State = #{module := Module}) ->
  {Json, ReqNew, StateNew} = to_json(Req, State),
  Title = Module:name(),
  Context = [
    {title, Title},
    {single_data, Json}
  ],
  {ok, ReqNew1, Body} = decirest_handler_lib:render(ReqNew, Json, Context),
  {Body, ReqNew1, StateNew}.

-spec to_json(_,#{'module':=atom(), _=>_}) -> any().
to_json(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_json, Req, State, fun to_json_default/2).

-spec to_json_default(_,#{'module':=_, 'rstate':=map(), _=>_}) -> {binary(),_,#{'child_fun':=fun((_) -> any()), 'module':=_, 'rstate':=map(), _=>_}}.
to_json_default(#{path := Path} = Req, State = #{module := Module}) ->
  Data = decirest:get_data(Module, State),
  Data1 = decirest_handler_lib:prepare_output(Data, State),
  Data2 = decirest_handler_lib:validate_output(single, Data1, State),
  ChildUrls = decirest:child_urls_map(decirest:get_children(Module), Req, State),
  PrettyConfig = decirest_handler_lib:maybe_pretty(Req, State),
  {jiffy:encode(maps:merge(ChildUrls, Data2#{self_url => Path}), [force_utf8] ++ PrettyConfig), Req, State}.

-spec resource_exists(_,#{'module':=atom(), _=>_}) -> any().
resource_exists(Req, State = #{module := Module}) ->
  decirest:apply_with_default(Module, resource_exists, [Req, State], fun resource_exists_default/2).

-spec resource_exists_default(_,#{'module':=_, _=>_}) -> any().
resource_exists_default(Req, State = #{mro_call := true, module := Module}) ->
  case decirest_handler_lib:fetch_data(Req, State) of
    {ok, [Data]} ->
      decirest_auth:gate2(Req, decirest:put_data(Module, Data, State));
    {ok, []} ->
      {false, Req, State};
    {ok, Data} when is_list(Data) ->
      ReqNew = cowboy_req:reply(409, Req),
      {stop, ReqNew, State};
    {ok, Data} ->
      decirest_auth:gate2(Req, decirest:put_data(Module, Data, State));
    {error, Reason} ->
      lager:debug("got exception when fetching data ~p ~p", [Module, Reason]),
      {false, Req, State};
    {StatusCode, NewState} when is_number(StatusCode) ->
      ReqNew = cowboy_req:reply(StatusCode, Req),
      {stop, ReqNew, NewState};
    {StatusCode, RespBody, NewState} when is_number(StatusCode) ->
      ReqNew = cowboy_req:reply(StatusCode, #{}, RespBody, Req),
      {stop, ReqNew, NewState}
  end;
resource_exists_default(Req, State = #{module := Module}) ->
  {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true),
  {maps:get(Module, Res, false), ReqNew, StateNew}.
