%%%-------------------------------------------------------------------
%%% @author jso
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2017 15:56
%%%-------------------------------------------------------------------
-module(decirest_example).
-author("jso").

%% API
-export([
  create_test_data/0,
  test_api/0
]).

create_test_data() ->
  Accounts = [
    #{
      id => 1,
      name => <<"Test account 1">>,
      description => <<"test account 1">>
    },
    #{
      id => 2,
      name => <<"Test account 2">>,
      description => <<"test account 2">>
    },
    #{
      id => 3,
      name => <<"Test account 3">>,
      description => <<"test account 3">>
    }
  ],
  ARes = ets:insert(account_db, [{ID, Obj} || #{id := ID} = Obj <- Accounts]),


  {ARes}.


test_api() ->
  ibrowse:start(),
  Proj = #{
      id => 1,
      name => <<"Test project">>,
      description => <<"test">>
    },
  User = #{
    id => 1,
    first_name => <<"john">>,
    last_name => <<"doe">>,
    email => <<"john.doe@example.com">>
  },
  R0 = post_obj("/bo/account/1/project", Proj, [1, 5, 9]),
  R1 = post_obj("/api/account/2/project", Proj, [1, 2, 3]),
  R2 = post_obj("/api/account/1/user/", User, [1,3,4]),
  R3 = post_obj("/api/account/2/user/", User, [1,2,3,4]),
  R4 = post_obj("/api/account/1/project/1/user/", User, [1,5,6]),
  R5 = post_obj("/api/account/1/project/2/user/", User, [2,5,7]),
  {R0, R1, R2, R3, R4, R5}.

post_obj(Path, Obj, Ids) ->
  post_obj(Path, Obj, Ids, []).

post_obj(Path, Obj, [Id | Ids], Results) ->
  {ok, Res, _, _} = post_obj(Path, Obj#{id => Id}),
  post_obj(Path, Obj, Ids, [Res | Results]);
post_obj(_, _, [], Results) ->
  lists:reverse(Results).

post_obj(Path, Obj) ->
  Body = jsx:encode(Obj),
  Hdrs = [{"content-type", "application/json"}],
  ibrowse:send_req("http://127.0.0.1:8081" ++ Path, Hdrs, post, Body).

