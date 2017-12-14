-module(project_r).

-export([
  name/0,
  ident/0,
  data_pk/0,
  child_of/0,
  fetch_data/2,
  persist_data/2,
  validate_payload/2
]).

name() -> <<"project">>.
ident() -> project_id.
data_pk() -> <<"id">>.

child_of() -> [account_r, user_r].

fetch_data(Filter, #{account_r := #{data:= #{<<"id">> := AccountId}}} = State) ->
  lager:error("project fetch data ~p", [State]),
  Data = case Filter of
           #{project_id := ID} ->
             Select = [{{{AccountId, binary_to_integer(ID)},'$1'},[],['$1']}],
             ets:select(project_db, Select);
           _ ->
             Select = [{{{AccountId,'_'},'$1'},[],['$1']}],
             ets:select(project_db, Select)
         end,
  {ok, Data};
fetch_data(_, State) ->
  lager:error("project error fetch data ~p", [State]),
  {ok, []}.

persist_data(#{<<"id">> := Id} = Payload, #{rstate := #{account_r := #{data:= #{<<"id">> := AccountId}}}} = State) ->
  case ets:insert(project_db, {{AccountId, Id}, Payload}) of
    true ->
      {ok, State};
    Error ->
      {error, Error}
  end.

schema() ->
  Priv = code:priv_dir(decirest_example),
  {ok, Bin} = file:read_file(filename:join(Priv, <<"project.json">>)),
  jsx:decode(Bin, [return_maps]).

validate_payload(Body, #{rstate := #{ project_r := #{data := #{<<"id">> := ID}}}}) ->
  Input = jsx:decode(Body, [return_maps]),
  lager:critical("input = ~p, schema = ~p", [Input, schema()]),
  case decirest_validator:validate_on_schema(Input, schema()) of
    {ok, #{<<"id">> := ID}} = Res ->
      Res;
    {ok, _} ->
      {error, #{<<"id">> => [<<"can't change this value">>]}};
    Error ->
      Error
  end;
validate_payload(Body, State) ->
  Input = jsx:decode(Body, [return_maps]),
  case State of
    #{module_binding := MB} ->
      decirest_validator:validate_on_schema(Input#{<<"id">> => binary_to_integer(MB)}, schema());
    _ ->
      decirest_validator:validate_on_schema(Input, schema())
  end.

