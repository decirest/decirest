-module(user_r).

-export([
  name/0,
  ident/0,
  data_pk/0,
  child_of/0,
  fetch_data/2,
  persist_data/2,
  delete_data/2,
  validate_payload/2,
  schema/0
]).

name() -> <<"user">>.
ident() -> user_id.
data_pk() -> <<"id">>.

child_of() -> [account_r, project_r].

fetch_data(#{user_id := BinId}, #{account_r := #{data:= #{<<"id">> := AccountId}}} = State) ->
  Id = binary_to_integer(BinId),
  case get_ids(State) of
    undefined ->
      Select = [{{{AccountId, Id},'$1'},[], ['$1']}],
      {ok, ets:select(user_db, Select)};
    Ids ->
      lager:critical("Ids = ~p, Id = ~p", [Ids, Id]),
      case [X || X <- Ids, X == Id] of
        [] ->
          {ok, []};
        _ ->
          Select = [{{{AccountId, Id},'$1'},[], ['$1']}],
          {ok, ets:select(user_db, Select)}
      end
  end;
fetch_data(_Filter, #{account_r := #{data:= #{<<"id">> := AccountId}}} = State) ->
  lager:error("user fetch data ~p", [State]),

  case get_ids(State) of
    undefined ->
      Select = [{{{AccountId, '_'},'$1'},[], ['$1']}],
      {ok, ets:select(user_db, Select)};
    Ids ->
      Select = [{{{AccountId, Id},'$1'},[], ['$1']} || Id <- Ids],
      {ok, ets:select(user_db, Select)}
  end;
fetch_data(_, State) ->
  lager:error("user error fetch data ~p", [State]),
  {ok, []}.

get_ids(#{account_r := #{data := #{<<"id">> := AccountId}}, project_r := #{data := #{<<"id">> := ProjectId}}}) ->
  Select = [{{AccountId, ProjectId, '$1'}, [], ['$1']}],
  ets:select(m2m_project_user, Select);
get_ids(_State) ->
  undefined.

persist_data(#{<<"id">> := Id} = Payload, #{rstate := RState} = State) ->
  #{account_r := #{data:= #{<<"id">> := AccountId}}} = RState,
  E0 = case persist_data(#{<<"relate_to_project">> => [Id]}, State) of
         {ok, _} ->
           [];
         {error, Err} ->
           Err
       end,
  Error = case ets:insert(user_db, {{AccountId, Id}, Payload}) of
            true ->
              [];
            E ->
              [E | E0]
          end,
  case Error of
    [] ->
      {ok, State};
    _ ->
      {error, Error}
  end;
persist_data(#{<<"relate_to_project">> := Ids} = Payload, #{rstate := RState} = State) ->
  #{account_r := #{data:= #{<<"id">> := AccountId}}} = RState,
  ProjectId = case {Payload, RState} of
                {#{<<"project_id">> := Pid}, _} -> Pid;
                {_, #{project_r := #{data := #{<<"id">> := Pid}}}} -> Pid;
                {_, _} -> undefined
              end,
  case ProjectId of
    undefined -> {ok, State};
    ProjectId ->
      case ets:insert(m2m_project_user, [{AccountId, ProjectId, Id} || Id <- Ids]) of
        true ->
          {ok, State};
        Error ->
          {error, Error}
      end
  end.

delete_data(Req = #{bindings := #{user_id := UId}}, State = #{rstate := #{account_r := #{data := #{<<"id">> := AccountId}}}}) ->
  UserId = binary_to_integer(UId),
  case State of
    #{rstate := #{project_r := #{data := #{<<"id">> := ProjectId}}}} ->
      {ets:match_delete(m2m_project_user, {AccountId, ProjectId, UserId}), Req, State};
    #{} ->
      A = ets:match_delete(m2m_project_user, {AccountId, '_', UserId}),
      B = ets:delete(user_db, {AccountId, UserId}),
      lager:critical("~n~n~p~n~n", [{A, B}]),
      {true, Req, State}

  end.

schema() ->
  Priv = code:priv_dir(decirest_example),
  {ok, Bin} = file:read_file(filename:join(Priv, <<"user.json">>)),
  jsx:decode(Bin, [return_maps]).

validate_payload(Body, #{method := <<"PATCH">>, rstate := #{ user_r := #{data := Data = #{<<"id">> := Id}}}}) ->
  Input = jsx:decode(Body, [return_maps]),
  lager:critical("user patch input = ~p, schema = ~p", [Input, schema()]),
  case decirest_validator:validate_on_schema(maps:merge(Data, Input), schema()) of
    {ok, #{<<"id">> := Id}} = Res ->
      Res;
    {ok, _} ->
      {error, #{<<"id">> => [<<"can't change this value">>]}};
    Error ->
      Error
  end;
validate_payload(Body, #{rstate := #{ user_r := #{data := #{<<"id">> := Id}}}}) ->
  Input = jsx:decode(Body, [return_maps]),
  lager:critical("user input = ~p, schema = ~p", [Input, schema()]),
  case decirest_validator:validate_on_schema(Input, schema()) of
    {ok, #{<<"id">> := Id}} = Res ->
      Res;
    {ok, _} ->
      {error, #{<<"id">> => [<<"can't change this value">>]}};
    Error ->
      Error
  end;
validate_payload(Body, State) ->
  Input = jsx:decode(Body, [return_maps]),

  lager:critical("validate input = ~p, State = ~p", [Input, State]),
  case State of
    #{module_binding := MB} ->
      decirest_validator:validate_on_schema(Input#{<<"id">> => binary_to_integer(MB)}, schema());
    _ ->
      decirest_validator:validate_on_schema(Input, schema())
  end.

