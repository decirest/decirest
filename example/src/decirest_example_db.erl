-module(decirest_example_db).

-export([
  init_dbs/0,
  create_test_data/0

]).


init_dbs() ->
  ets:new(account_db, [set, named_table, public]),
  ets:new(project_db, [set, named_table, public]),
  ets:new(user_db, [set, named_table, public]),
  ets:new(todo_db, [set, named_table, public]),
  ets:new(m2m_project_user, [bag, named_table, public]),
  ets:new(m2m_project_todo, [bag, named_table, public]),
  ets:new(m2m_user_todo, [bag, named_table, public]),
  ok.

create_test_data() ->
  Accounts = [
    #{
      <<"id">> => 1,
      <<"name">> => <<"Test account 1">>,
      <<"description">> => <<"test account 1">>
    },
    #{
      <<"id">> => 2,
      <<"name">> => <<"Test account 2">>,
      <<"description">> => <<"test account 2">>
    },
    #{
      <<"id">> => 3,
      <<"name">> => <<"Test account 3">>,
      <<"description">> => <<"test account 3">>
    }
  ],
  ARes = ets:insert(account_db, [{ID, Obj} || #{<<"id">> := ID} = Obj <- Accounts]),

  {ARes}.
