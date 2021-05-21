-module(decirest_auth).
-export([
  is_authorized/2,
  forbidden/2,
  authenticate/2,
  gate1/2,
  gate2/2,
  gate3/2
]).

-spec is_authorized(_,_) -> any().
is_authorized(Req0, State0) ->
  case authenticate(Req0, State0) of
    {ok, Req, State} ->
      {true, Req, State};
    Res ->
      Res
  end.

-spec forbidden(_,_) -> any().
forbidden(Req0, State0) ->
  case gate1(Req0, State0) of
    {true, Req, State} ->
      {false, Req, State};
    {false, Req, State} ->
      {true, Req, State}
  end.

-spec authenticate(_,_) -> any().
authenticate(Req, State = #{module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, authenticate, [Req, State], fun AuthModule:authenticate/2);
authenticate(Req, State) ->
  {true, Req, State}.

-spec gate1(_,_) -> any().
gate1(Req, State = #{module := Module, decirest_auth_module := AuthModule}) ->
  case maps:get(main_module, State) of
    Module ->
      decirest:apply_with_default(Module, gate1, [Req, State], fun AuthModule:gate1/2);
    _ ->
      Method = decirest_req:method(Req),
      MroReq = decirest_req:set_method(Req, <<"GET">>),
      {B, NewReq, NewState} = decirest:apply_with_default(Module, gate1, [MroReq, State], fun AuthModule:gate1/2),
      {B, decirest_req:set_method(NewReq, Method), NewState}
  end;
gate1(Req, State) ->
  {true, Req, State}.


-spec gate2(_,_) -> any().
gate2(Req, State = #{module := Module, decirest_auth_module := AuthModule}) ->
  case maps:get(main_module, State) of
    Module ->
      decirest:apply_with_default(Module, gate2, [Req, State], fun AuthModule:gate2/2);
    _ ->
      Method = decirest_req:method(Req),
      MroReq = decirest_req:set_method(Req, <<"GET">>),
      {B, NewReq, NewState} = decirest:apply_with_default(Module, gate2, [MroReq, State], fun AuthModule:gate2/2),
      {B, decirest_req:set_method(NewReq, Method), NewState}
  end;
gate2(Req, State) ->
  {true, Req, State}.

-spec gate3(_,_) -> any().
gate3(Req, State = #{module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, gate3, [Req, State], fun AuthModule:gate3/2);
gate3(Req, State) ->
  {true, Req, State}.
