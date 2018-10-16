%%%-------------------------------------------------------------------
%%% @author jso
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2017 18:57
%%%-------------------------------------------------------------------
-module(create_account_r).
-author("jso").

%% API
-export([
  name/0,
  paths/0,
  child_of/0,
  allowed_methods/2,
  from_fun/2
]).

name() -> <<"create_account">>.
paths() -> [{"/create_account", decirest_single_handler}].
child_of() -> [bo_r].

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

from_fun(Req, State) ->
  {true, Req, State}.
