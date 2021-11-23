%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2018 08:18
%%%-------------------------------------------------------------------
-module(decirest_handler_lib).
-author("mikael").

%% API
-export([
  init_rest/2,
  init_rest_default/2,
  is_authorized/2,
  is_authorized_default/2,
  forbidden/2,
  forbidden_default/2,
  content_types_accepted/2,
  content_types_accepted_default/2,
  export_to_methods/3,
  is_exported/3,
  fetch_data/2,
  delete_data/2,
  validate_action/3,
  validate_payload/3,
  return_error/3,
  persist_data/3,
  perform_action/3,
  options/2,
  options_default/2,
  maybe_pretty/2,
  to_csv/2,
  add_default_allow_header/2,
  prepare_output/2,
  validate_output/3,
  from_multi/2,
  from_multi_default/2,
  render/3
]).

-spec init_rest(_, map()) -> {'cowboy_rest', _, #{rstate := #{}}}.
init_rest(Req, State) ->
  decirest:do_callback(init, Req, State, fun init_rest_default/2).

init_rest_default(Req, State) ->
  {cowboy_rest, Req#{bindings => decirest_query:get_bindings(Req, State)}, State#{rstate => #{}}}.

%init_rest_default(Req, #{mro_call := true} = State) ->
%  {cowboy_rest, Req#{bindings => decirest_query:get_bindings(Req, State)}, State};
%init_rest_default(Req, #{module := Module} = State) ->
%  {Res, ReqNew, StateNew} = decirest:call_mro(init_rest, Req, State#{rstate => #{}}, cowboy_rest, always_true),
%  {maps:get(Module, Res, false), ReqNew, StateNew}.

-spec is_authorized(_, #{module := atom(), _ => _}) -> any().
is_authorized(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, is_authorized, Req, State, fun is_authorized_default/2).

-spec is_authorized_default(_, _) -> any().
is_authorized_default(Req, State) ->
  decirest_auth:is_authorized(Req, State).

-spec forbidden(_, #{module := atom(), _ => _}) -> any().
forbidden(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, forbidden, Req, State, fun forbidden_default/2).

-spec forbidden_default(_, map()) -> any().
forbidden_default(Req, State = #{mro_call := true}) ->
  decirest_auth:forbidden(Req, State);
forbidden_default(Req, State = #{module := Module}) ->
  Continue = inverted,
  {Res, ReqNew, StateNew} = decirest:call_mro(forbidden, Req, State, false, Continue),
  {maps:get(Module, Res, true), ReqNew, StateNew}.

-spec content_types_accepted(_, #{'module' := atom(), _ => _}) -> any().
content_types_accepted(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, content_types_accepted, Req, State, fun content_types_accepted_default/2).

-spec content_types_accepted_default(_, _) -> {[{{_, _, _}, 'from_fun'}, ...], _, _}.
content_types_accepted_default(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, from_fun},
    {{<<"application">>, <<"javascript">>, '*'}, from_fun}
  ], Req, State}.

%%------------------------------------------------------------------------------
%% @doc export_to_methods
%% Function to map exported function to http methods
%%
%% If the call back module exports validate_payload we will add POST as allowed method
%% [{{validate_payload,  [2, 3]}, [<<"POST">>]}]
%%
%% @end
%%------------------------------------------------------------------------------

export_to_methods(Module, ExportMappingList, DefaultMethods) ->
  lists:foldl(
    fun({{Function, Arity}, Methods}, Acc) ->
      case is_exported(Module, Function, Arity) of
        true ->
          lists:append([Methods, Acc]);
        false ->
          Acc
      end
    end, DefaultMethods, ExportMappingList).

is_exported(Module, Function, ArityList) when is_list(ArityList) ->
  lists:any(fun(R) -> R end,
    [is_exported(Module, Function, Arity) || Arity <- ArityList]
  );

is_exported(Module, Function, Arity) ->
  erlang:function_exported(Module, Function, Arity).

fetch_data(Req, State = #{module := Module}) ->
  Bindings = decirest_req:bindings(Req),
  case is_exported(Module, fetch_data, 3) of
    true ->
      Module:fetch_data(Bindings, Req, State);
    false ->
      Module:fetch_data(Bindings, State)
  end.

delete_data(Req, State = #{module := Module}) ->
  Bindings = decirest_req:bindings(Req),
  case is_exported(Module, delete_data, 3) of
    true ->
      Module:delete_data(Bindings, Req, State);
    false ->
      Module:delete_data(Bindings, State)
  end.

validate_action(Body, Req, State) ->
  validate_action_parts(Body, Req, State).

validate_payload(Body, Req, State) ->
  validate_payload(normal, Body, Req, State).

validate_payload(_Type, Body, Req, State = #{module := Module}) ->
  case is_exported(Module, validate_payload, 3) of
    true ->
      Module:validate_payload(Body, Req, State);
    false ->
      case is_exported(Module, validate_payload, 2) of
        true ->
          Module:validate_payload(Body, State);
        false ->
          %% Gives the call back the posibility to override parts of
          %% the validate code and let decirest handle the rest
          validate_parts(Body, Req, State)
      end
  end.

validate_parts(Body, Req, State) ->
  ValidateCalls =
    [
      {validate_bin, fun validate_bin/1},
      {to_term, fun to_term/1},
      {validate_on_schema, validate_on_schema_fun(schema)},
      {validate_term, fun validate_term/1}
    ],
  unwrap_epipe(epipe:run(ValidateCalls, {Body, Req, State})).

validate_action_parts(Body, Req, State) ->
  ValidateCalls =
    [
      {to_term, fun to_term/1},
      {validate_on_schema, validate_on_schema_fun(action_schema)},
      {validate_action, fun validate_action/1}
    ],
  unwrap_epipe(epipe:run(ValidateCalls, {Body, Req, State})).


validate_bin({Body, Req, State = #{module := Module}}) ->
  Res = decirest:apply_with_default(Module, validate_bin, [Body, Req, State], fun validate_bin_default/3),
  epipe_resp(Res, Req, State).

validate_bin_default(Body, _Req, _State) ->
  {ok, Body}.

to_term({Body, Req, State = #{module := Module}}) ->
  Res = decirest:apply_with_default(Module, to_term, [Body, Req, State], fun to_term_default/3),
  epipe_resp(Res, Req, State).

to_term_default(Body, _Req, _State) ->
  decirest_validator:json_decode(Body).

validate_on_schema_fun(SchemaCallback) ->
  fun({Term, Req, State = #{module := Module}}) ->
    Res =
      case is_exported(Module, SchemaCallback, 0) of
        true ->
          decirest:apply_with_default(Module, validate_on_schema, [Term, Req, State], validate_on_schema_default(SchemaCallback));
        false ->
          {ok, Term}
      end,
    epipe_resp(Res, Req, State)
  end.

validate_on_schema_default(SchemaCallback) ->
  fun(Term, _Req, _State = #{module := Module}) ->
    decirest_validator:validate_on_schema(Term, Module:SchemaCallback())
  end.

validate_term({Term, Req, State = #{module := Module}}) ->
  Res = decirest:apply_with_default(Module, validate_term, [Term, Req, State], fun validate_term_default/3),
  epipe_resp(Res, Req, State).

validate_term_default(Term, _Req, _State) ->
  {ok, Term}.

validate_action({Term, Req, State = #{module := Module}}) ->
  Res = decirest:apply_with_default(Module, validate_action, [Term, Req, State], fun validate_action_default/3),
  epipe_resp(Res, Req, State).

validate_action_default(Term, _Req, _State) ->
  {ok, Term}.

%% Return same output as input and not put that requirement on callbacks
epipe_resp({ok, Res}, Req, State) ->
  {ok, {Res, Req, State}};
epipe_resp(Error, _Req, _State) ->
  Error.

unwrap_epipe({ok, {Res, _Req, _State}}) ->
  {ok, Res};
unwrap_epipe({error, _Fun, Error, _}) ->
  {error, Error}.

return_error(Errors, Req, State) ->
  lager:critical("errors ~p", [Errors]),
  RespBody = jiffy:encode(Errors, [force_utf8]),
  ReqNew = decirest_req:set_resp_body(RespBody, Req),
  {false, ReqNew, State}.

-spec persist_data(map(), map(), #{'module' := atom(), _ => _}) -> any().
persist_data(Body, Req, State = #{module := Module}) ->
  case is_exported(Module, persist_data, 3) of
    true ->
      Module:persist_data(Body, Req, State);
    false ->
      Module:persist_data(Body, State)
  end.

perform_action(Body, Req, State = #{module := Module}) ->
  case is_exported(Module, perform_action, 3) of
    true ->
      Module:perform_action(Body, Req, State);
    false ->
      Module:perform_action(Body, State)
  end.

options(Req, State = #{module := Module}) ->
  case decirest:do_callback(Module, options, Req, State, fun options_default/2) of
    no_call ->
      no_call;
    {ok, Req1, State1} ->
      case cowboy_req:has_resp_header(<<"allow">>, Req1) of
        true ->
          {ok, Req1, State1};
        false ->
          add_default_allow_header(Req1, State1)
      end
  end.

to_csv(_Name, Map) ->
  lists:map(fun({K, V}) ->
    make_csv_row([decirest:t2b(K), ";"], V)
            end, maps:to_list(Map)).


make_csv_row(Prefix, Map) when is_map(Map) ->
  lists:map(fun({K, V}) ->
    make_csv_row([Prefix, decirest:t2b(K), ";"], V)
            end, maps:to_list(Map));

make_csv_row(Prefix, List) when is_list(List) ->
  lists:map(fun(V) ->
    make_csv_row(Prefix, V)
            end, List);

make_csv_row(Prefix, Value) ->
  [Prefix, decirest:t2b(Value), <<"\n">>].


options_default(_Req, _State) ->
  %% Use cowboys internal return to indicate that the function
  %% is not exported and cowboy should use its default options response
  no_call.

maybe_pretty(Req, State = #{module := Module}) ->
  case decirest:do_callback(Module, pretty, Req, State, false) of
    true ->
      [pretty];
    false ->
      [];
    {false, _, _} ->
      []
  end.

prepare_output(DataList, State) when is_list(DataList) ->
  [prepare_output(Data, State) || Data <- DataList];
prepare_output(Data, #{module := Module}) ->
  decirest:apply_with_default(Module, prepare_output, [Data], Data).

validate_output(Type, DataList, State) when is_list(DataList) ->
  [validate_output(Type, Data, State) || Data <- DataList];
validate_output(Type, Data, #{module := Module}) ->
  case is_exported(Module, output_schema, 0) of
    true ->
      case get_schema(Type, Module) of
        no_schema ->
          Data;
        Schema ->
          case decirest_validator:validate_on_schema(Data, Schema) of
            {error, Errors} ->
              lager:error("Validation errors: ~p ~p", [Errors, Data]),
              Data#{validation_errors => Errors};
            {ok, ValidData} ->
              ValidData
          end
      end;
    false ->
      Data
  end.

get_schema(Type, Module) ->
  case Module:output_schema() of
    #{Type := Schema} when is_map(Schema) ->
      Schema;
    Schema when is_map(Schema) ->
      Schema;
    _ ->
      no_schema
  end.

add_default_allow_header(Req, #{allowed_methods := Methods} = State) ->
  <<", ", Allow/binary>> = <<<<", ", M/binary>> || M <- Methods>>,
  {ok, decirest_req:set_resp_header(<<"allow">>, Allow, Req), State}.

from_multi(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, from_multi, Req, State, fun from_multi_default/2).

from_multi_default(Req, State) ->
  {ok, Headers, Req2} = cowboy_req:read_part(Req),
  {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
  {file, Input, Filename, ContentType}
    = cow_multipart:form_data(Headers),
  {{Filename, Input, ContentType, Data}, Req3, State}.

render(Req, Json, Context) ->
  case cowboy_req:binding(render, Req, true) of
    false ->
      NewReq = decirest_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
      {ok, NewReq, Json};
    _ ->
      {ok, Body} = std_response_html_dtl:render(Context),
      {ok, Req, Body}
  end.
