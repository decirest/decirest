-module(decirest).
-export([
  build_routes/1, build_routes/2,
  call_mro/3, call_mro/4, call_mro/5,
  child_fun_factory/1,
  child_url/3,
  child_urls_map/3,
  is_ansestor/2,
  module_pk/1,
  do_callback/5,
  apply_with_default/4,
  pretty_path/1,
  t2b/1
]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

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

child_url(Module, #{path := Path}, _State) ->
  ChildPath = case erlang:function_exported(Module, paths, 0) of
                true ->
                  case Module:paths() of
                    [{P, _} | _] ->
                      P;
                    [{P, _, _} | _] ->
                      P
                  end;
                false ->
                  Module:name()
              end,
  pretty_path([Path, "/", ChildPath]).

child_urls_map(Children, Req, State) ->
  child_urls_map(Children, Req, State, #{}).

child_urls_map([Child | Children], Req, State, Map) ->
  case do_callback(Child, forbidden, Req, State, false) of
    {false, _, _} ->
      case apply_with_default(Child, child_url, [Child, Req, State], fun child_url/3) of
        #{} = Res ->
          child_urls_map(Children, Req, State, maps:merge(Map, Res));
        Url ->
          Key = << (Child:name())/binary, "_url">>,
          child_urls_map(Children, Req, State, Map#{Key => Url})
      end;
    {true, _, _} ->
      child_urls_map(Children, Req, State, Map)
  end;
child_urls_map([], _Req, _State, Map) ->
  Map.

pretty_path(Path) when is_binary(Path) ->
  case binary:replace(Path, <<"//">>, <<"/">>, [global]) of
    Path ->
      Path;
    NewPath ->
      pretty_path(NewPath)
  end;
pretty_path(Path) when is_list(Path) ->
  pretty_path(iolist_to_binary(Path)).

call_mro(Callback, Req, State) ->
  call_mro(Callback, Req, State, undefined, fun(_) -> true end).

call_mro(Callback, Req, State, Default) ->
  call_mro(Callback, Req, State, Default, fun(_) -> true end).

call_mro(Callback, Req, State = #{mro := MRO, module := Module}, Default, Continue) ->
  {Res, NewReq, NewState} = call_mro(MRO, Callback, Req, State#{mro_call => true}, Default, Continue, #{}),
  {Res, NewReq, NewState#{mro_call => false, module => Module}}.

call_mro([{Handler, Mod} | MRO], Callback, Req0, State0, Default, Continue, Res0) ->
  {ModRes, Req, State} = CallbackRes = do_callback(Handler, Callback, Req0, State0#{module => Mod}, Default),
  Res = Res0#{Mod => ModRes},
  case Continue(CallbackRes) of
    true ->
      call_mro(MRO, Callback, Req, State, Default, Continue, Res);
    false ->
      {Res, Req, State}
  end;
call_mro([], _Callback, Req, State, _Default, _Continue, Res) ->
  {Res, Req, State}.

is_ansestor(Module, #{mro := MRO}) ->
  case [true || {_, M} <- MRO, M == Module] of
    [] ->
      false;
    [_ | _] ->
      true
  end.

module_pk(Module) ->
  case erlang:function_exported(Module, data_pk, 0) of
    true ->
      Module:data_pk();
    false ->
      id
  end.

do_callback(Mod, Callback, Req, State, Default) ->
  lager:debug("do callback ~p", [Callback]),
  case erlang:function_exported(Mod, Callback, 2) of
    true ->
      lager:debug("Executing Module ~p:s callback", [Mod]),
      Mod:Callback(Req, State);
    false ->
      lager:debug("Executing default callback"),
      case is_function(Default) of
        true ->
          % TODO: we don't send Mod, the more specified MRO in state should be enough
          % Default(Mod, Req, State);
          Default(Req, State);
        false ->
          {Default, Req, State}
      end
  end.

apply_with_default(M, F, A, Default) ->
  case erlang:function_exported(M, F, length(A)) of
    true ->
      erlang:apply(M, F, A);
    false ->
      case is_function(Default) of
        true ->
          erlang:apply(Default, A);
        false ->
          Default
      end
  end.

t2b(V) when is_integer(V) -> integer_to_binary(V);
t2b(V) when is_list(V) -> list_to_binary(V);
t2b(V) when is_atom(V) -> atom_to_binary(V, utf8);
t2b(V) when is_binary(V) -> V.

-ifdef(TEST).

child_url_test() ->
  Req = #{
    scheme => <<"http">>, host => <<"localhost">>, port => 8080,
    path => Path = <<"/api/v1/company/1/">>, qs => <<"dummy=2785">>
  },
  ChildPath = "user",
  Path = cowboy_req:uri(Req, #{host => undefined}),
  <<"/api/v1/company/1/user">> = pretty_path([Path, "/", ChildPath]),
  ok.

-endif.
