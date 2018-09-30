%%%-------------------------------------------------------------------
%%% @author jso
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2017 18:56
%%%-------------------------------------------------------------------
-module(api_r).
-author("jso").

%% API
-export([
  name/0,
  paths/0,
  child_of/0,
  fetch_data/2
]).

name() -> <<"api">>.

paths() -> [{"/api", decirest_single_handler}].
child_of() -> [].

fetch_data(_Filter, _State) ->
  {ok, #{description => <<"the API">>}}.
