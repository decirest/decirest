%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2018 14:32
%%%-------------------------------------------------------------------
-module(decirest_router_test).

-author("mikael").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

child_test() ->
    meck:new(a, [non_strict]),
    mock_name(a, <<"a">>),
    mock_paths(a, [{"/a/:a", decirest_single_handler}]),
    mock_child_of(a, []),

    meck:new(b, [non_strict]),
    mock_name(b, <<"b">>),
    mock_child_of(b, [a]),
    mock_ident(b, b_id),

    ?assert(lists:keymember("/a/:a",
                            1,
                            extract_paths(decirest_router:build_routes(test_ref, [a, b], #{})))),
    ?assert(lists:keymember(["/a/:a", "/", <<"b">>],
                            1,
                            extract_paths(decirest_router:build_routes(test_ref, [a, b], #{})))),

    ?assertNot(lists:keymember("/a/:a",
                               1,
                               extract_paths(decirest_router:build_routes(test_ref, [b], #{})))),
    ?assert(lists:keymember(["/a/:a", "/", <<"b">>],
                            1,
                            extract_paths(decirest_router:build_routes(test_ref, [b], #{})))).

mock_name(Mod, Name) -> meck:expect(Mod, name, 0, Name).

mock_paths(Mod, Paths) -> meck:expect(Mod, paths, 0, Paths).

mock_child_of(Mod, Parents) -> meck:expect(Mod, child_of, 0, Parents).

mock_ident(Mod, Ident) -> meck:expect(Mod, ident, 0, Ident).

extract_paths([Hd | _]) ->
    {_, _, Paths} = Hd,
    Paths.
