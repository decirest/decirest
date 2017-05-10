-module(decirest).
-export([
  build_routes/1, build_routes/2,
  call_mro/3, call_mro/4, call_mro/5,
  child_fun_factory/1
]).


build_routes(Mod) ->
  build_routes(Mod, decirest_handler).

build_routes(Mod, Handler) when is_atom(Mod)->
  %lists:flatten(build_routes_path(Mod:paths(), Handler, Mod));
  build_routes([Mod], Handler, child_fun_factory([Mod]), []);
build_routes(Modules, Handler) ->
  build_routes(Modules, Handler, child_fun_factory(Modules), []).

build_routes([Mod | Modules], Handler, ChildFun, Res) ->
  MRes = build_routes_path(Mod:paths(), Handler, Mod),
  build_routes(Modules, Handler, ChildFun, [MRes | Res]);
build_routes([], _Handler, ChildFun, Res) ->
  Routes = lists:flatten(Res),
  [{P, H, S#{children => ChildFun(M)}} || {P, H, S = #{module := M}} <- Routes].

build_routes_with_state(Mod, Handler, State = #{mro := MRO}) when is_map(State) ->
  case sets:is_element(Mod, sets:from_list(MRO)) of
    true ->
      [];
    false ->
      lists:flatten(build_routes_path(Mod:paths(), Handler, State#{mro => [Mod | MRO], module => Mod}))
  end.

build_routes_path(Paths, Handler, Mod) when is_atom(Mod) ->
  build_routes_path(Paths, Handler, #{mro => [Mod] , module => Mod}, []);
build_routes_path(Paths, Handler, State) when is_map(State) ->
  build_routes_path(Paths, Handler, State, []).

build_routes_path([Path | Paths], Handler, State = #{module := Mod}, Res0) ->
  io:format("br paths ~p, ~p~n", [Path, State]),
  case Mod:child_of() of
    [] ->
      build_routes_path(Paths, Handler, State, [{Path, Handler, State} | Res0]);
    Parents ->
      Res = build_routes_parent(Parents, {Path, Handler, State}, Res0),
      build_routes_path(Paths, Mod, Handler, Res)
  end;
build_routes_path([], _Handler, _State, Res) ->
  Res.

build_routes_parent([Parent | Parents], Cfg = {_, Handler, State}, Res) ->
  io:format("br parent, ~p, ~p~n", [Parent, Cfg]),
  build_routes_parent(Parents, Cfg, [merge_routes(build_routes_with_state(Parent, Handler, State), Cfg) | Res]);
build_routes_parent([], _Cfg, Res) ->
  Res.

merge_routes(ParentRoutes, Cfg) ->
  merge_routes(ParentRoutes, Cfg, []).

merge_routes([ParentRoute | ParentRoutes], Cfg = {Path, Handler, #{module := Mod}}, Res0) ->
  {ParentPath, _, PState} = ParentRoute,
      Res = [{[ParentPath | Path], Handler, PState#{module => Mod}} | Res0],
      merge_routes(ParentRoutes, Cfg, Res);
merge_routes([], _Cfg, Res) ->
  Res.

child_fun_factory(Resources) ->
  FFun = fun(CR, Acc) -> [[{R, CR} || R <- erlang:apply(CR, child_of, [])] | Acc] end,
  CsList = lists:flatten(lists:foldl(FFun, [], Resources)),
  fun(PR) -> proplists:get_all_values(PR, CsList) end.

call_mro(Callback, Req, State) ->
  call_mro(Callback, Req, State, undefined, fun(_) -> true end).

call_mro(Callback, Req, State, Default) ->
  call_mro(Callback, Req, State, Default, fun(_) -> true end).

call_mro(Callback, Req, State = #{mro := MRO}, Default, Continue) ->
  call_mro(MRO, Callback, Req, State, Default, Continue, #{}).

call_mro([Mod | MRO], Callback, Req0, State0, Default, Continue, Res0) ->
  {ModRes, Req, State} = CallbackRes = do_callback(Mod, Callback, Req0, State0, Default),
  Res = Res0#{Mod => ModRes},
  case Continue(CallbackRes) of
    true ->
      call_mro(MRO, Callback, Req, State, Default, Continue, Res);
    false ->
      {Res, Req, State}
  end;
call_mro([], _Callback, Req, State, _Default, _Continue, Res) ->
  {Res, Req, State}.

do_callback(Mod, Callback, Req, State, Default) ->
  case erlang:function_exported(Mod, Callback, 2) of
    true ->
      Mod:Callback(Req, State);
    false ->
      case is_function(Default) of
        true ->
          Default(Mod, Req, State);
        false ->
          {Default, Req, State}
      end
  end.

