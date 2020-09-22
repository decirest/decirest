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
  is_exported/3,
  fetch_data/2,
  delete_data/2,
  validate_payload/3,
  persist_data/3,
  options/2,
  options_default/2,
  maybe_pretty/2,
  to_csv/2,
  add_default_allow_header/2,
  from_multi/2,
  from_multi_default/2
]).

-spec init_rest(_, map()) -> {'cowboy_rest', _, #{rstate := #{}}}.
init_rest(Req, State = #{module := Module}) ->
  decirest:do_callback(init, Req, State, fun init_rest_default/2).

init_rest_default(Req, State) ->
  {cowboy_rest, Req#{bindings => decirest_query:get_bindings(Req, State)}, State#{rstate => #{}}}.

-spec is_authorized(_, #{module := atom(), _ => _}) -> any().
is_authorized(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, is_authorized, Req, State, fun is_authorized_default/2).

-spec is_authorized_default(_,_) -> any().
is_authorized_default(Req, State) ->
  decirest_auth:is_authorized(Req, State).

-spec forbidden(_, #{module := atom(), _ => _}) -> any().
forbidden(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, forbidden, Req, State, fun forbidden_default/2).

-spec forbidden_default(_,map()) -> any().
forbidden_default(Req, State = #{mro_call := true}) ->
  decirest_auth:forbidden(Req, State);
forbidden_default(Req, State = #{module := Module}) ->
  Continue = inverted,
  {Res, ReqNew, StateNew} = decirest:call_mro(forbidden, Req, State, false, Continue),
  {maps:get(Module, Res, true), ReqNew, StateNew}.

-spec content_types_accepted(_,#{'module':=atom(), _=>_}) -> any().
content_types_accepted(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, content_types_accepted, Req, State, fun content_types_accepted_default/2).

-spec content_types_accepted_default(_,_) -> {[{{_,_,_},'from_fun'},...],_,_}.
content_types_accepted_default(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, from_fun},
    {{<<"application">>, <<"javascript">>, '*'}, from_fun}
  ], Req, State}.


is_exported(Module, Function, ArityList) when is_list(ArityList) ->
  lists:any(fun(R) -> R end,
    [is_exported(Module, Function, Arity) || Arity <- ArityList ]
  );

is_exported(Module, Function, Arity) ->
  erlang:function_exported(Module, Function, Arity).

fetch_data(Req, State = #{module := Module}) ->
  Bindings = cowboy_req:bindings(Req),
  case is_exported(Module, fetch_data, 3) of
    true ->
      Module:fetch_data(Bindings, Req, State);
    false ->
      Module:fetch_data(Bindings, State)
  end.

delete_data(Req, State = #{module := Module}) ->
  Bindings = cowboy_req:bindings(Req),
  case is_exported(Module, delete_data, 3) of
    true ->
      Module:delete_data(Bindings, Req, State);
    false ->
      Module:delete_data(Bindings, State)
  end.

-spec validate_payload(binary(), map(), #{'module':=atom(), 'module_binding':=_, _=>_}) -> any().
validate_payload(Body, Req, State = #{module := Module}) ->
  case is_exported(Module, validate_payload, 3) of
    true ->
      Module:validate_payload(Body, Req, State);
    false ->
      Module:validate_payload(Body, State)
  end.

-spec persist_data(binary(), map(), #{'module':=atom(), _=>_}) -> any().
persist_data(Body, Req, State = #{module := Module}) ->
  case is_exported(Module, persist_data, 3) of
    true ->
      Module:persist_data(Body, Req, State);
    false ->
      Module:persist_data(Body, State)
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

to_csv(Name, Map) ->
  lists:map(fun({K, V}) ->
    make_csv_row([decirest:t2b(K), ";"], V)
           end, maps:to_list(Map)).


make_csv_row(Prefix, Map) when is_map(Map) ->
  lists:map(fun({K, V}) ->
     make_csv_row([Prefix , decirest:t2b(K), ";"], V)
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

add_default_allow_header(Req, #{allowed_methods := Methods} = State) ->
  <<", ", Allow/binary>> = <<<<", ", M/binary>> || M <- Methods>>,
  {ok, cowboy_req:set_resp_header(<<"allow">>, Allow, Req), State}.

from_multi(Req, State = #{module := Module}) ->
  decirest:do_callback(Module, from_multi, Req, State, fun from_multi_default/2).

from_multi_default(Req, State) ->
  {ok, Headers, Req2} = cowboy_req:read_part(Req),
  {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
  {file, Input, Filename, ContentType}
    = cow_multipart:form_data(Headers),
  {{Filename, Input, ContentType, Data}, Req3, State}.
