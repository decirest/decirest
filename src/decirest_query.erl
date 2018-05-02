%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2018 16:00
%%%-------------------------------------------------------------------
-module(decirest_query).
-author("mikael").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  get_bindings/2]).


get_bindings(Req, #{module := Module} = State) ->
  lager:error("Req == :~p  ", [Req]),
  Bindings = cowboy_req:bindings(Req),
  case erlang:function_exported(Module, query_bindings, 0) of
    false ->
      maybe_get_query_matches(Req, State);
    true ->
      QueryMatch = Module:query_bindings(),
      QueryBindings = cowboy_req:match_qs(QueryMatch, Req),
      MergedBindings = maps:merge(QueryBindings, Bindings),
      check_for_duplicate_bindings(QueryBindings, Bindings, MergedBindings),
      MergedBindings
  end.

maybe_get_query_matches(Req, #{module := Module} = State) ->
  Bindings = cowboy_req:bindings(Req),
  case erlang:function_exported(Module, get_query_bindings, 2) of
    true ->
      QueryBindings = Module:get_query_bindings(Req, State),
      MergedBindings = maps:merge(QueryBindings, Bindings),
      check_for_duplicate_bindings(QueryBindings, Bindings, MergedBindings),
      MergedBindings;
    false ->
      Bindings
  end.

check_for_duplicate_bindings(QueryBindings, Bindings, MergedBindings) ->
  case maps:size(QueryBindings) + maps:size(Bindings) == maps:size(MergedBindings) of
    true ->
      ok;
    false ->
      QueryBindingsSet = sets:from_list(maps:keys(QueryBindings)),
      BindingsSet = sets:from_list(maps:keys(Bindings)),
      DuplicateBindings = sets:to_list(sets:intersection(QueryBindingsSet, BindingsSet)),
      lager:error("Same binding was used for both url binding and query_bindings: ~p", [DuplicateBindings])
  end.


-ifdef(TEST).
query_bindings_test() ->
  meck:new(query_module, [non_strict]),
  meck:expect(query_module, query_bindings, 0,   [{timestamp_from, [], 1}, {timestamp_to, [], 1000}]),
  Req = #{bindings => #{ a => <<"cowboybinding">>}, qs => <<"timestamp_from=16457000000000&timestamp_to=16467000000000">>},
  Bindings = get_bindings(Req, #{module => query_module}),
  ?assertEqual(<<"cowboybinding">>, maps:get(a, Bindings)),
  ?assertEqual(<<"16457000000000">>, maps:get(timestamp_from, Bindings)),
  ?assertEqual(<<"16467000000000">>, maps:get(timestamp_to, Bindings)),
  meck:unload().

default_query_bindings_test() ->
  meck:new(query_module, [non_strict]),
  meck:expect(query_module, query_bindings, 0, [{timestamp_from, [], 1}, {timestamp_to, [], 1000}]),
  Req = #{bindings => #{ a => <<"cowboybinding">>}, qs => <<>>},
  Bindings = get_bindings(Req, #{module => query_module}),
  ?assertEqual(<<"cowboybinding">>, maps:get(a, Bindings)),
  ?assertEqual(1, maps:get(timestamp_from, Bindings)),
  ?assertEqual(1000, maps:get(timestamp_to, Bindings)),
  meck:unload().

get_query_bindings_test() ->
  meck:new(query_module, [non_strict]),
  meck:expect(query_module, get_query_bindings, 2,
    fun(Req, State) ->
      cowboy_req:match_qs([{timestamp_from, [], 1}, {timestamp_to, [], 1000}], Req)
    end),
  Req = #{bindings => #{ a => <<"cowboybinding">>}, qs => <<"timestamp_from=11&timestamp_to=12">>},
  Bindings = get_bindings(Req, #{module => query_module}),
  ?assertEqual(<<"cowboybinding">>, maps:get(a, Bindings)),
  ?assertEqual(<<"11">>, maps:get(timestamp_from, Bindings)),
  ?assertEqual(<<"12">>, maps:get(timestamp_to, Bindings)),
  meck:unload().
-endif.
