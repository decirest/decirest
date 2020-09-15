-module(decirest_collection_handler).
-export([
  init/2,
  is_authorized/2,
  forbidden/2,
  allow_missing_post/2, allow_missing_post_default/2,
  allowed_methods/2, allowed_methods_default/2,
  options/2,
  content_types_accepted/2,
  content_types_provided/2,
  from_fun/2, from_fun_default/2,
  from_multi/2,
  to_fun/2, to_fun_default/2,
  to_html/2, to_html_default/2,
  to_json/2, to_json_default/2,
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

allow_missing_post(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, allow_missing_post, Req, State, fun allow_missing_post_default/2).

allow_missing_post_default(Req,State) ->
  {false, Req, State}.

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
  Methods =
    case decirest_handler_lib:is_exported(Module, validate_payload, [2, 3]) of
      true ->
        [<<"POST">>];
      false ->
        []
    end,
  {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">> | Methods], Req, State}.

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

-spec from_fun_default(#{'path':=_, _=>_},#{'module':=atom(), _=>_}) -> {'false' | 'stop' | {'true',binary()},map(),_}.
from_fun_default(Req0, State) ->
  % gate 2 here
  {ok, Body, Req} = cowboy_req:read_body(Req0),
  handle_body(Body, Req, State).

handle_body(Body, Req = #{path := Path}, State = #{module := Module}) ->
  PK = decirest:module_pk(Module),
  case decirest_handler_lib:validate_payload(Body, Req, State) of
    {ok, Payload = #{PK := ID}} ->
      % gate3 auth here
      case decirest_handler_lib:persist_data(Payload, Req, State) of
        {ok, NewState} ->
          SelfUrl = decirest:pretty_path([Path, "/", decirest:t2b(ID)]),
          {{true, SelfUrl}, Req, NewState};
        {ok, #{PK := NewID}, NewState} ->
          SelfUrl = decirest:pretty_path([Path, "/", decirest:t2b(NewID)]),
          {{true, SelfUrl}, Req, NewState};
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
    {error, Errors} ->
      lager:critical("errors ~p", [Errors]),
      RespBody = jiffy:encode(Errors, [force_utf8]),
      ReqNew = cowboy_req:set_resp_body(RespBody, Req),
      {false, ReqNew, State}
  end.

-spec content_types_provided(_,#{'module':=atom(), _=>_}) -> any().
content_types_provided(Req, State = #{module := Module}) ->
  Default = [
    {{<<"application">>, <<"json">>, '*'}, to_json},
    {{<<"application">>, <<"javascript">>, '*'}, to_json},
    {{<<"text">>, <<"html">>, '*'}, to_html},
    {{<<"application">>, <<"octet-stream">>, '*'}, to_fun}
  ],
  decirest:do_callback(Module, content_types_provided,Req, State, Default).

-spec to_fun(_,#{'module':=atom(), _=>_}) -> any().
to_fun(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_fun, Req, State, fun to_fun_default/2).

-spec to_fun_default(_,#{'module':=atom(), _=>_}) -> any().
to_fun_default(Req, State) ->
  to_json(Req, State).

-spec to_html(_,#{'module':=atom(), _=>_}) -> any().
to_html(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_html, Req, State, fun to_html_default/2).

-spec to_html_default(_,#{'module':=atom(), _=>_}) -> {_,_,_}.
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

-spec to_json(_,#{'module':=atom(), _=>_}) -> any().
to_json(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, to_json, Req, State, fun to_json_default/2).

to_json_default(Req, State) ->
  Data0 = fetch_data(Req, State),
  Data = filter_data_on_pk(Data0, Req, State),
  PrettyConfig = decirest_handler_lib:maybe_pretty(Req, State),
  {jiffy:encode(Data, [force_utf8] ++ PrettyConfig), Req, State}.

fetch_data(Req, #{module := Module} = State) ->
  case Module:fetch_data(cowboy_req:bindings(Req), State) of
    {ok, D} ->
      D;
    {error, Msg} ->
      lager:debug("got exception when fetching data ~p", [Msg]),
      []
  end.

filter_data_on_pk(Data, Req, State = #{module := Module}) ->
  Children = decirest:get_children(Module),
  PK =
    case decirest_handler_lib:is_exported(Module, data_pk, 0) of
      true ->
        Module:data_pk();
      false ->
        id
    end,
  [data_prep(D, PKVal, Children, Req, State) || D = #{PK := PKVal} <- Data].

-spec data_prep(map(),_,_,#{'path'=>binary() | maybe_improper_list(any(),binary() | []) | byte(), _=>_},#{'child_fun':=_, 'module':=_, 'rstate':=_, _=>_}) -> map().
data_prep(Data, PKVal, Children, Req0 = #{path := Path}, State) ->
  SelfUrl = get_self_url(Path, Data, PKVal),
  Req = Req0#{path => SelfUrl},
  ChildUrls = decirest:child_urls_map(Children, Req, State),
  maps:merge(ChildUrls, maps:remove(self_url_partials, Data#{self_url => SelfUrl}));

data_prep(Data, _, _, Req, _) ->
  lager:error("prep failure, ~p", [Req]),
  Data.

get_self_url(Path, Data, PKVal) ->
  case maps:get(self_url_partials, Data, undefined) of
    undefined ->
      decirest:pretty_path([Path, "/", decirest:t2b(PKVal)]);
    {Replace, Add} ->
      replace_and_add(Replace, Add, Path)
  end.

replace_and_add(Replace, Add, Path) ->
  case hd(string:split(Path, <<"/", Replace/binary>>)) of
    [] ->
      error(wrong_self);
    SelfBase ->
      <<SelfBase/binary, "/", Add/binary>>
  end.

-spec resource_exists(_,#{'module':=atom(), _=>_}) -> any().
resource_exists(Req, State = #{module := Module}) ->
  decirest:apply_with_default(Module, resource_exists, [Req, State], fun resource_exists_default/2).

-spec resource_exists_default(_,map()) -> {_,_,#{'mro_call':=boolean(), _=>_}}.
resource_exists_default(Req, State = #{mro_call := true}) ->
  {true, Req, State};
resource_exists_default(Req, State = #{module := Module}) ->
  {Res, ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true),
  {maps:get(Module, Res, false), ReqNew, StateNew}.
