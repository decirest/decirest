-module(decirest_router).
-export([
  build_routes/1, build_routes/2,
  get_paths/2
]).

-spec get_paths(atom(),map()) -> [{_,_,map()}].
get_paths(Module, Options) ->
  case erlang:function_exported(Module, paths, 0) of
    true ->
      prep_paths(Module:paths(), Options#{pp_mod => Module});
    false ->
      prep_paths(make_paths(Module, Options), Options#{pp_mod => Module})
  end.

-spec make_paths(atom(),map()) -> [{[any(),...],_,map()},...].
make_paths(Module, #{single_handler := SH, collection_handler := CH}) ->
  Name = Module:name(),
  Ident = atom_to_list(Module:ident()),
  [{["/", Name], CH, #{children => false}}, {["/", Name, "/:", Ident], SH, #{}}];
make_paths(Module, Options) ->
  SH = maps:get(single_handler, Options, decirest_single_handler),
  CH = maps:get(collection_handler, Options, decirest_collection_handler),
  make_paths(Module, Options#{single_handler => SH, collection_handler => CH}).

-spec prep_paths([{_,_} | {_,_,map()}],#{'pp_mod':=atom(), _=>_}) -> [{_,_,map()}].
prep_paths(Paths, Options) ->
  prep_paths(Paths, Options, []).

-spec prep_paths([{_,_} | {_,_,map()}],#{'pp_mod':=atom(), _=>_},[{_,_,map()}]) -> [{_,_,map()}].
prep_paths([PathDef | Paths], Options = #{pp_mod := Module, state := State}, Res) ->
  {Path, Handler, PState} = case PathDef of
                          {P, H, S} ->
                            MRO = maps:get(mro, S, [{H, Module}]),
                            {P, H, S#{module => Module, mro => MRO}};
                          {P, H} ->
                            {P, H, #{module => Module, mro => [{H, Module}]}}
                        end,
  prep_paths(Paths, Options, [{Path, Handler, maps:merge(State, PState)} | Res]);
prep_paths([], _Options, Res) ->
  Res.


-spec build_routes(_) -> [{'_',[],[any()]},...].
build_routes(Modules) ->
  build_routes(Modules, #{}).

-spec build_routes(_,map()) -> [{'_',[],[any()]},...].
build_routes(Modules, Options) when is_list(Modules) ->
  State = maps:get(state, Options, #{}),
  ChildFun = decirest:child_fun_factory(Modules),
  build_routes(Modules, Options#{state => State#{child_fun => ChildFun}}, []);
build_routes(Module, Options) ->
  State = maps:get(state, Options, #{}),
  ChildFun = decirest:child_fun_factory([Module]),
  build_routes([Module], Options#{state => State#{child_fun => ChildFun}}, []).

-spec build_routes([any()],#{'state':=#{'child_fun':=fun((_) -> any()), _=>_}, _=>_},[any()]) -> [{'_',[],[any()]},...].
build_routes([Module | Modules], Options, Res) ->
  build_routes(Modules, Options, [build_module_routes(Module, Options) | Res]);
build_routes([], #{nostatic := true}, Res) ->
  [{'_', [], lists:flatten(Res)}];
build_routes([], _Options, Res) ->
  [{'_', [], lists:flatten([{"/assets/[...]", cowboy_static, {priv_dir, decirest, "static/assets"}} | Res])}].

-spec build_module_routes(_,#{'state':=map(), _=>_}) -> any().
build_module_routes(Module, Options) ->
  Modules = maps:get(modules, Options, []),
  State = maps:get(state, Options, #{}),
  case sets:is_element(Module, sets:from_list(Modules)) of
    true ->
      []; % break possible endless recursion if resources are child of each other
    false ->
      case erlang:function_exported(Module, get_routes, 1) of
        true ->
          Module:get_routes(Options#{state => State#{main_module => Module}});
        false ->
          br(Module, Options#{state => State#{main_module => Module}, modules => [Module | Modules]})
      end
  end.


-spec br(atom(),#{'modules':=[any(),...], 'state':=#{'main_module':=atom(), _=>_}, _=>_}) -> [[{_,_,_}] | {_,_,map()}].
br(Module, Options) ->
Paths = get_paths(Module, Options),
  case Module:child_of() of
    [] ->
      Paths;
    Parents ->
      merge_with_parents(Parents, Paths, Options, [])
  end.

-spec merge_with_parents([any()],[{_,_,map()}],#{'modules':=[any(),...], 'state':=#{'main_module':=atom(), _=>_}, _=>_},[[{_,_,_}]]) -> [[{_,_,_}]].
merge_with_parents([Parent | Parents], Paths, Options, Res) ->
  ParentRoutes = lists:flatten(build_module_routes(Parent, Options#{imaparent => true})),
  merge_with_parents(Parents, Paths, Options, merge_with_parent(ParentRoutes, Paths, Res));
merge_with_parents([], _Paths, _Options, Res) ->
  Res.

-spec merge_with_parent([{_,_,[any()] | map()}],[{_,_,map()}],[[{_,_,_}]]) -> [[{_,_,_}]].
merge_with_parent([{_Path, _Handler, #{children := false}} | ParentPaths], Paths, Res) ->
  merge_with_parent(ParentPaths, Paths, Res);
merge_with_parent([{Path, _Handler, #{} = PState} | ParentPaths], Paths, Res) ->
  %NewPaths = [{[Path | P], H, maps:merge(PState, maps:without([mro], S))} || {P, H, S} <- Paths],
  NewPaths = [{[Path | P], H, state_merge(PState, S)} || {P, H, S} <- Paths],
  merge_with_parent(ParentPaths, Paths, [NewPaths | Res]);
merge_with_parent([{'_', _, ParentPaths} | Routes], Paths, Res) ->
  merge_with_parent(Routes, Paths, merge_with_parent(ParentPaths, Paths, Res));
merge_with_parent([], _Paths, Res) ->
  Res.

-spec state_merge(#{'mro':=[any()], _=>_},#{'mro':=_, _=>_}) -> #{'mro':=_, _=>_}.
state_merge(ParentState = #{mro := PMRO}, ModuleState = #{mro := MMRO}) ->
  State = maps:merge(ParentState, ModuleState),
  State#{mro => PMRO ++ MMRO}.

