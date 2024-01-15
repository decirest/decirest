-module(decirest).

-export([start/4,
         get_modules/1,
         call_mro/4,
         call_mro/5,
         get_children/1,
         child_fun_factory/1,
         child_url/3,
         child_urls_map/3,
         is_ancestor/2,
         module_pk/1,
         do_callback/4,
         do_callback/5,
         apply_with_default/4,
         pretty_path/1,
         get_parent/1,
         get_parent_pk/1,
         main_module/1,
         t2b/1,
         get_data/2,
         get_data/3,
         get_data/4,
         put_data/3,
         change_module/2,
         get_handler/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-type host() :: string() | atom().
-type state() ::
    #{decirest_auth_module := module(),
      main_module => module(),
      mro => [{module(), module()}],
      module => module(),
      children => boolean() | hidden,
      _ => _}.
-type opts() ::
    #{hosts => [host()],
      state => state(),
      nostatic => boolean(),
      all_modules => [module()],
      pp_mod => module(),
      single_handler => module(),
      collection_handler => module(),
      modules => [module()],
      _ => _}.

-export_type([host/0,
              opts/0,
              state/0]).

-spec start(Ref, ModulesIn, DecirestOpt, TransportOpt) -> Res
    when Ref :: ranch:ref(),
         ModulesIn :: [module()],
         DecirestOpt :: opts(),
         TransportOpt :: ranch:opts(),
         Res :: {ok, pid()} | {error, any()}.
start(Ref, ModulesIn, DecirestOpt, TransportOpt) ->
    Modules = [decirest_doc_resource | ModulesIn],
    Routes = decirest_router:build_routes(Ref, Modules, DecirestOpt),
    Dispatch = cowboy_router:compile(Routes),
    store_modules(Ref, Modules),
    cowboy:start_clear(Ref,
                       TransportOpt,
                       #{env => #{dispatch => Dispatch},
                         stream_handlers => [cowboy_compress_h, cowboy_stream_h]}).

store_modules(Ref, Modules) -> ok = persistent_term:put({?MODULE, Ref}, Modules).

get_modules(Ref) -> persistent_term:get({?MODULE, Ref}).

get_children(Resource) -> persistent_term:get({?MODULE, Resource}, []).

clear_resource(Resource) -> persistent_term:erase({?MODULE, Resource}).

-spec child_fun_factory([module()]) -> ok.
child_fun_factory(Resources) ->
    lists:foreach(fun clear_resource/1, Resources),
    lists:foreach(fun add_resource_as_child/1, Resources).

add_resource_as_child(Resource) ->
    lists:foreach(fun(Parent) -> add_child(Parent, Resource) end, Resource:child_of()).

add_child(Parent, Child) ->
    case is_children_visible(Parent) of
        true ->
            PrevChildren = get_children(Parent),
            persistent_term:put({?MODULE, Parent}, [Child | PrevChildren]);
        _ -> ok
    end.

-spec child_url(atom(),
                #{path := binary() | maybe_improper_list(any(), binary() | []) | byte(),
                  _ => _},
                _) ->
                   binary().
child_url(Module, #{path := Path}, _State) ->
    ChildPath =
        case erlang:function_exported(Module, paths, 0) of
            true ->
                case Module:paths() of
                    [{P, _} | _] -> P;
                    [{P, _, _} | _] -> P
                end;
            false -> Module:name()
        end,
    pretty_path([Path, "/", ChildPath]).

is_children_visible(Module) ->
    case erlang:function_exported(Module, paths, 0) of
        true ->
            case Module:paths() of
                [{_, _} | _] -> true;
                [{_, _, #{children := false}} | _] -> false;
                [{_, _, #{children := hidden}} | _] -> false;
                [{_, _, _} | _] -> true
            end;
        false -> true
    end.

-spec child_urls_map([atom()], _, _) -> map().
child_urls_map(Children, Req, State) -> child_urls_map(Children, Req, State, #{}).

-spec child_urls_map([atom()], _, _, map()) -> map().
child_urls_map([Child | Children], Req, State, Map) ->
    case do_callback(Child, forbidden, Req, State, false) of
        {false, _, _} ->
            case apply_with_default(Child, child_url, [Child, Req, State], fun child_url/3) of
                #{} = Res -> child_urls_map(Children, Req, State, maps:merge(Map, Res));
                Url ->
                    Key = <<(Child:name())/binary, "_url">>,
                    child_urls_map(Children, Req, State, Map#{Key => Url})
            end;
        {true, _, _} -> child_urls_map(Children, Req, State, Map)
    end;
child_urls_map([], _Req, _State, Map) -> Map.

-spec pretty_path(binary() |
                  maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | byte(),
                                      binary() | [])) ->
                     binary().
pretty_path(Path) when is_binary(Path) ->
    case binary:replace(Path, <<"//">>, <<"/">>, [global]) of
        Path -> Path;
        NewPath -> pretty_path(NewPath)
    end;
pretty_path(Path) when is_list(Path) -> pretty_path(iolist_to_binary(Path)).

-spec call_mro(_,
               _,
               #{module := _,
                 mro := maybe_improper_list(),
                 _ => _},
               _) ->
                  {map(),
                   _,
                   #{module := _,
                     mro_call := false,
                     _ => _}}.
call_mro(Callback, Req, State, Default) ->
    call_mro(Callback, Req, State, Default, default).

-spec call_mro(_,
               _,
               #{module := _,
                 mro := maybe_improper_list(),
                 _ => _},
               _,
               _) ->
                  {map(),
                   _,
                   #{module := _,
                     mro_call := false,
                     _ => _}}.
call_mro(Callback,
         Req,
         State =
             #{mro := MRO,
               module := Module},
         Default,
         ContinueType) ->
    {Res, NewReq, NewState} =
        call_mro(MRO, Callback, Req, State#{mro_call => true}, Default, ContinueType, #{}),
    {Res,
     NewReq,
     NewState#{mro_call => false,
               module => Module}}.

-spec call_mro(maybe_improper_list(), _, _, _, _, _, map()) -> {map(), _, _}.
call_mro([{Handler, Mod} | MRO], Callback, Req0, State0, Default, ContinueType, Res0) ->
    {ModRes, Req, State} =
        CallbackRes = do_callback(Handler, Callback, Req0, State0#{module => Mod}, Default),
    Res = Res0#{Mod => ModRes},
    case continue(ContinueType, CallbackRes) of
        true -> call_mro(MRO, Callback, Req, State, Default, ContinueType, Res);
        false -> {Res, Req, State}
    end;
call_mro([], _Callback, Req, State, _Default, _ContinueType, Res) -> {Res, Req, State}.

continue(always_true, _) -> true;
continue(inverted, {true, _, _}) -> false;
continue(inverted, {false, _, _}) -> true;
continue(default, {true, _, _}) -> true;
continue(default, {false, _, _}) -> false.

-spec is_ancestor(atom(), map()) -> true | false.
is_ancestor(Module, #{mro := MRO}) -> lists:keymember(Module, 2, MRO).

get_parent(#{mro := MRO,
             module := CurrentModule}) ->
    get_parent(lists:reverse(MRO), CurrentModule).

get_parent([{_, CurrentModule}, {_, Parent} | _Tail], CurrentModule) -> Parent;
get_parent([{_, _}, {Handler, Parent} | Tail], CurrentModule) ->
    get_parent([{Handler, Parent} | Tail], CurrentModule).

get_parent_pk(State) ->
    Parent = get_parent(State),
    {Parent, module_pk(Parent)}.

main_module(State) -> maps:get(main_module, State).

-spec module_pk(atom()) -> any().
module_pk(Module) ->
    case erlang:function_exported(Module, data_pk, 0) of
        true -> Module:data_pk();
        false -> id
    end.

% If a header "trace" is in the request, log everything going through this module
maybe_trace_result(Module,
                   Callback,
                   #{headers := #{<<"trace">> := Symbol}} = Req,
                   State,
                   Result) ->
    {R, Req1, State1} = Result,
    lager:info("Trace ~p:~p with symbol ~p\nRequest:\n~p\nState:\n~p\nResponse:\n~p\n",
               [Module, Callback, Symbol, Req, State, R]),
    case Symbol of
        <<"full">> -> lager:info("New Req:\n~p\nNew State:\n~p\n\n\n", [Req1, State1]);
        _ -> lager:info("\n\n")
    end,
    Result;
maybe_trace_result(_, _, _, _, Result) -> Result.

-spec do_callback(atom(), atom(), _, _, _) -> any().
do_callback(Callback, Req, #{module := Module} = State, Default) ->
    do_callback(Module, Callback, Req, State, Default).

do_callback(Module, Callback, Req, State, Default) ->
    Result =
        case erlang:function_exported(Module, Callback, 2) of
            true ->
                case Module:Callback(Req, State) of
                    {run_default, [Req1, State1]} ->
                        case is_function(Default) of
                            true -> erlang:apply(Default, [Req1, State1]);
                            false -> {Default, Req1, State1}
                        end;
                    Res -> Res
                end;
            false ->
                case is_function(Default) of
                    true ->
                        % TODO: we don't send Mod, the more specified MRO in state should be enough
                        Default(Req, State);
                    false -> {Default, Req, State}
                end
        end,
    maybe_trace_result(Module, Callback, Req, State, Result).

-spec apply_with_default(atom(), atom(), [any()], _) -> any().
apply_with_default(M, F, A, Default) ->
    case erlang:function_exported(M, F, length(A)) of
        true ->
            case erlang:apply(M, F, A) of
                %% UpdatedA should be same length/type as A
                {run_default, UpdatedA} ->
                    case is_function(Default) of
                        true -> erlang:apply(Default, UpdatedA);
                        false -> exit(run_default_not_a_function)
                    end;
                Res -> Res
            end;
        false ->
            case is_function(Default) of
                true -> erlang:apply(Default, A);
                false -> Default
            end
    end.

-spec get_data(atom(), map()) -> any().
get_data(Module, #{rstate := RState}) -> get_data(Module, RState);
get_data(Module, State) ->
    case State of
        #{Module := #{data := Data}} -> Data;
        _ -> undefined
    end.

put_data(Module, Data, State = #{rstate := RState}) ->
    State#{rstate => RState#{Module => #{data => Data}}}.

-spec get_data(any(), atom(), map()) -> any().
get_data(Key, Module, State) when is_atom(Module) ->
    get_data(Key, Module, State, undefined).

-spec get_data(any(), atom(), map(), any()) -> any().
get_data(Key, Module, State, Default) ->
    case get_data(Module, State) of
        #{Key := Val} -> Val;
        #{} = Data ->
            case proplists:get_value(t2b(Key), [{t2b(K), K} || K <- maps:keys(Data)]) of
                undefined -> Default;
                RawKey -> maps:get(RawKey, Data)
            end;
        _ -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc makes it possible to reroute to a different module
%%      Can be used to implement api versions
%%      Should be used in init/2 in handler implementations
%%
%%      init(Req, State) ->
%%          {run_default, [Req, decirest:change_module(different_modules, State)]}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec change_module(Module, State) -> State
    when Module :: atom(),
         State :: map().
%% Decirest state
change_module(Module, #{mro := MRO} = State) ->
    [{Handler, _OldModule} | Tail] = lists:reverse(MRO),
    State#{module => Module,
           main_module => Module,
           mro => lists:reverse([{Handler, Module} | Tail])}.

-spec get_handler(State) -> Handler
    when State :: map(),
         Handler :: decirest_single_handler | decirest_collection_handler.
                   % Decirest state

get_handler(#{module := Module,
              mro := MRO} =
                _State) ->
    {Handler, Module} = lists:last(MRO),
    Handler.

-spec t2b(atom() |
          binary() |
          maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | byte(),
                              binary() | []) |
          integer()) ->
             binary().
t2b(V) when is_integer(V) -> integer_to_binary(V);
t2b(V) when is_list(V) -> list_to_binary(V);
t2b(V) when is_atom(V) -> atom_to_binary(V, utf8);
t2b(V) when is_binary(V) -> V.

-ifdef(TEST).

child_url_test() ->
    Path = <<"/api/v1/company/1/">>,
    Req = #{scheme => <<"http">>,
            host => <<"localhost">>,
            port => 8080,
            path => Path,
            qs => <<"dummy=2785">>},
    ChildPath = "user",
    ?assert(lists:member(Path, decirest_req:uri(Req))),
    ?assertEqual(pretty_path([Path, "/", ChildPath]), <<"/api/v1/company/1/user">>).

get_parent_test() ->
    State =
        #{mro =>
              [{decirest_single_handler, res_1},
               {decirest_single_handler, res_2},
               {decirest_single_handler, res_3},
               {decirest_single_handler, res_4},
               {decirest_single_handler, res5}],
          module => res_3},
    ?assertEqual(res_2, get_parent(State)).

-endif.
