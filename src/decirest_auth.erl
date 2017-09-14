-module(decirest_auth).
-export([
  is_authorized/2,
  forbidden/2,
  authenticate/2,
  gate1/2,
  gate2/2,
  gate3/2
]).

is_authorized(Req0, State0) ->
  case authenticate(Req0, State0) of
    {ok, Req, State} ->
      {true, Req, State};
    Res ->
      Res
  end.

forbidden(Req0, State0) ->
  case gate1(Req0, State0) of
    {true, Req, State} ->
      {false, Req, State};
    {false, Req, State} ->
      {true, Req, State};
    Res ->
      Res
  end.


authenticate(Req, State = #{module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, authenticate, [Req, State], fun AuthModule:authenticate/2);
authenticate(Req, State) ->
  {true, Req, State}.

gate1(Req, State = #{module := Module, decirest_auth_module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, gate1, [Req, State], fun AuthModule:gate1/2);
gate1(Req, State) ->
  {true, Req, State}.

gate2(Req, State = #{decirest_auth_module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, gate2, [Req, State], fun AuthModule:gate2/2);
gate2(Req, State) ->
  {true, Req, State}.

gate3(Req, State = #{decirest_auth_module := Module, decirest_auth_module := AuthModule}) ->
  decirest:apply_with_default(Module, gate3, [Req, State], fun AuthModule:gate3/2);
gate3(Req, State) ->
  {true, Req, State}.
