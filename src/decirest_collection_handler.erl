-module(decirest_collection_handler).
-export([
  init/2,
  is_authorized/2,
  forbidden/2,
  content_types_provided/2,
  to_fun/2, to_fun_default/2,
  to_html/2, to_html_default/2,
  to_json/2, to_json_default/2,
  resource_exists/2, resource_exists_default/2
]).

init(Req, State) ->
  lager:info("collection init ~p", [Req]),
  {cowboy_rest, Req, State#{rstate => #{}}}.

is_authorized(Req, State) ->
  decirest_auth:is_authorized(Req, State).

forbidden(Req, State) ->
  decirest_auth:forbidden(Req, State).

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
  PK = case erlang:function_exported(Module, data_pk, 0) of
         true ->
           Module:data_pk();
         false ->
           id
       end,
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
