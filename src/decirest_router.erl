-module(decirest_router).

-export([build_routes/2,
         build_routes/3,
         get_all_active_routes/1,
         get_all_active_routes/2]).

-type route_match() :: '_' | iodata().
-type route_path() ::
    {Path :: route_match(), Handler :: module(), State :: decirest:state()} |
    {Path :: route_match(), cowboy:fields(), Handler :: module(), State :: decirest:state()}.
-type route_rule() ::
    {Host :: route_match(), Paths :: [route_path()]} |
    {Host :: route_match(), cowboy:fields(), Paths :: [route_path()]}.
-type routes() :: [route_rule()].
-type handler() :: module().
-type path_opts() :: #{children => boolean() | hidden}.
-type path_def() :: {route_match(), handler()} | {route_match(), handler(), path_opts()}.

-spec get_paths(module(), decirest:opts()) -> [route_path()].
get_paths(Module, Options) ->
    case erlang:function_exported(Module, paths, 0) of
        true -> prep_paths(Module:paths(), Options#{pp_mod => Module});
        false -> prep_paths(make_paths(Module, Options), Options#{pp_mod => Module})
    end.

-spec make_paths(module(), decirest:opts()) -> [path_def()].
make_paths(Module,
           #{single_handler := SH,
             collection_handler := CH}) ->
    Name = Module:name(),
    Ident = atom_to_list(Module:ident()),
    [{["/", Name], CH, #{children => false}}, {["/", Name, "/:", Ident], SH, #{}}];
make_paths(Module, Options) ->
    SH = maps:get(single_handler, Options, decirest_single_handler),
    CH = maps:get(collection_handler, Options, decirest_collection_handler),
    make_paths(Module,
               Options#{single_handler => SH,
                        collection_handler => CH}).

-spec prep_paths([path_def()], decirest:opts()) -> [route_path()].
prep_paths(Paths, Options) -> prep_paths(Paths, Options, []).

-spec prep_paths([path_def()], decirest:opts(), [route_path()]) -> [route_path()].
prep_paths([PathDef | Paths],
           Options =
               #{pp_mod := Module,
                 state := State},
           Res) ->
    {Path, Handler, PState} =
        case PathDef of
            {P, H, S} ->
                MRO = maps:get(mro, S, [{H, Module}]),
                {P,
                 H,
                 S#{module => Module,
                    mro => MRO}};
            {P, H} ->
                {P,
                 H,
                 #{module => Module,
                   mro => [{H, Module}]}}
        end,
    prep_paths(Paths, Options, [{Path, Handler, maps:merge(State, PState)} | Res]);
prep_paths([], _Options, Res) -> Res.

-spec build_routes(Ref :: ranch:ref(), Modules :: [module()]) -> routes().
build_routes(Ref, Modules) -> build_routes(Ref, Modules, #{}).

-spec build_routes(Ref :: ranch:ref(),
                   Modules :: [module()],
                   Options :: decirest:opts()) ->
                      routes().
build_routes(Ref, Modules, Options) when is_list(Modules) ->
    State = maps:get(state, Options, #{}),
    ok = decirest:child_fun_factory(Modules),
    Hosts = maps:get(hosts, Options, ['_']),

    UpdatedOptions =
        Options#{hosts => Hosts,
                 all_modules => Modules,
                 state => State#{ref => Ref}},

    do_build_routes(Modules, UpdatedOptions, []).

-spec do_build_routes([module()], decirest:opts(), [[route_path()]]) -> routes().
do_build_routes([Module | Modules], Options, Res) ->
    do_build_routes(Modules, Options, [build_module_routes(Module, Options) | Res]);
do_build_routes([],
                #{nostatic := true,
                  hosts := Hosts},
                Res) ->
    [{Host, [], lists:flatten(Res)} || Host <- Hosts];
do_build_routes([], #{hosts := Hosts}, Res) ->
    [{Host,
      [],
      lists:flatten([{"/assets/[...]", cowboy_static, {priv_dir, decirest, "static/assets"}}
                     | Res])}
     || Host <- Hosts].

-spec build_module_routes(module(), decirest:opts()) -> [[route_path()]].
build_module_routes(Module, Options) ->
    Modules = maps:get(modules, Options, []),
    State = maps:get(state, Options, #{}),
    case lists:member(Module, Modules) of
        true ->
            []; % break possible endless recursion if resources are child of each other
        false ->
            case erlang:function_exported(Module, get_routes, 1) of
                true -> Module:get_routes(Options#{state => State#{main_module => Module}});
                false ->
                    build_route(Module,
                                Options#{state => State#{main_module => Module},
                                         modules => [Module | Modules]})
            end
    end.

-spec build_route(module(), decirest:opts()) -> [[route_path()]].
build_route(Module, Options) ->
    Paths = get_paths(Module, Options),
    case Module:child_of() of
        [] -> Paths;
        Parents ->
            AllModules = maps:get(all_modules, Options, []),
            case Parents -- AllModules of
                [] -> merge_with_parents(Parents, Paths, Options, []);
                Missing ->
                    lager:error("Parents missing in decirest modules ~w for ~w", [Missing, Module]),
                    merge_with_parents(Parents, Paths, Options, [])
            end
    end.

-spec merge_with_parents([module()], [route_path()], decirest:opts(), [[route_path()]]) ->
                            [[route_path()]].
merge_with_parents([Parent | Parents], Paths, Options, Res) ->
    ParentRoutes = lists:flatten(build_module_routes(Parent, Options#{imaparent => true})),
    merge_with_parents(Parents, Paths, Options, merge_with_parent(ParentRoutes, Paths, Res));
merge_with_parents([], _Paths, _Options, Res) -> Res.

-spec merge_with_parent(Parents :: [route_path()],
                        Child :: [route_path()],
                        Acc :: [[route_path()]]) ->
                           [[route_path()]].
merge_with_parent([{_Path, _Handler, #{children := false}} | ParentPaths], Paths, Res) ->
    merge_with_parent(ParentPaths, Paths, Res);
merge_with_parent([{Path, _Handler, #{} = PState} | ParentPaths], Paths, Res) ->
    NewPaths = [{[Path | P], H, state_merge(PState, S)} || {P, H, S} <- Paths],
    merge_with_parent(ParentPaths, Paths, [NewPaths | Res]);
% I don't believe this is a needed case
%merge_with_parent([{'_', _, ParentPaths} | Routes], Paths, Res) ->
%  merge_with_parent(Routes, Paths, merge_with_parent(ParentPaths, Paths, Res));
merge_with_parent([], _Paths, Res) -> Res.

-spec state_merge(decirest:state(), decirest:state()) -> decirest:state().
state_merge(ParentState = #{mro := PMRO}, ModuleState = #{mro := MMRO}) ->
    State = maps:merge(ParentState, ModuleState),
    State#{mro => PMRO ++ MMRO}.

get_all_active_routes(Ref, Name) ->
    [Route || Route <- decirest_router:get_all_active_routes(Ref), lists:member(Name, Route)].

get_all_active_routes(Ref) ->
    RanchOptions = ranch:get_protocol_options(Ref),
    Routes = hd(maps:get(dispatch, maps:get(env, RanchOptions))),
    lists:sort([Route || {Route, _, _, _} <- element(3, Routes)]).
