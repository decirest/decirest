-module(account_r).

-export([
  name/0,
  ident/0,
  data_pk/0,
  child_of/0,
  fetch_data/2,
  persist_data/2,
  validate_payload/2,
  schema/0
]).

name() -> <<"account">>.
ident() -> account_id.
data_pk() -> <<"id">>.

child_of() -> [bo_r, api_r].

fetch_data(Filter, _State) ->
  RawData = case Filter of
              #{account_id := ID} ->
                io:format("~n~p~n", [ID]),
                ets:lookup(account_db, binary_to_integer(ID));
              _ ->
                ets:select(account_db, [{'$1', [], ['$1']}])
            end,
  {ok, [Obj || {_ID, Obj} <- RawData]}.


persist_data(#{<<"id">> := ID} = Payload, State) ->
  case ets:insert(account_db, {ID, Payload}) of
    true ->
      {ok, State};
    Error ->
      {error, Error}
  end.

schema() ->
  Priv = code:priv_dir(decirest_example),
  {ok, Bin} = file:read_file(filename:join(Priv, <<"account.json">>)),
  jsx:decode(Bin, [return_maps]).

validate_payload(Body, #{rstate := #{ account_r := #{data := #{<<"id">> := ID}}}}) ->
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


