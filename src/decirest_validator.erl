-module(decirest_validator).
-export([
  validate_on_schema/2,
  validate_on_schema/3,
  validate_values/2
]).


-spec validate_on_schema(_,_) -> {'error',_} | {'ok',_}.
validate_on_schema(Obj, Schema) ->
  validate_on_schema(Obj, Schema, #{strict => false}). % false during test only

-spec validate_on_schema(_,_,_) -> {'error',_} | {'ok',_}.
validate_on_schema(Obj, Schema, Options) ->
  case jesse:validate_with_schema(Schema, Obj, [{allowed_errors, infinity}]) of
    {ok, Val} ->
      {ok, Val};
    {error, Errors} ->
      case Options of
        #{strict := false} ->
          ExtraProperties = [Path || {data_invalid, _, no_extra_properties_allowed, _, Path} <- Errors],
          validate_on_schema(drop_extra_properties(ExtraProperties, Obj), Schema, Options#{strict => true});
        _ ->
          {error, prettify_errors(Errors)}
      end
  end.


-spec drop_extra_properties([[any(),...]],_) -> any().
drop_extra_properties([Path | Paths], Obj) ->
  drop_extra_properties(Paths, drop_extra_property(Path, Obj));
drop_extra_properties([], Obj) ->
  Obj.

-spec drop_extra_property([any(),...],map()) -> map().
drop_extra_property([PathPart | []], Obj) ->
  maps:without([PathPart], Obj);
drop_extra_property([PathPart | PathParts], Obj) ->
  Obj#{PathPart => drop_extra_property(PathParts, maps:get(PathPart, Obj))}.


-spec prettify_errors([{'data_invalid',_,_,_,_}]) -> any().
prettify_errors(Errors) ->
  prettify_errors(Errors, #{}).

-spec prettify_errors([{'data_invalid',_,_,_,_}],_) -> any().
prettify_errors([{data_invalid, #{<<"required">> := Required}, missing_required_property, Value, Path} | Errors], ErrorMap) ->
  prettify_errors(Errors, add_required_errors(Required, Value, Path, ErrorMap));
prettify_errors([{data_invalid, _Schema, ErrorType, Value, Path} | Errors], ErrorMap) ->
  P = get_path(Path),
  prettify_errors(Errors, ErrorMap#{P => [[ErrorType, Value] | maps:get(P, ErrorMap, [])]});
prettify_errors([], ErrorMap) ->
  ErrorMap.

-spec add_required_errors([any()],_,_,_) -> any().
add_required_errors([R | Required], Value, Path, ErrorMap) ->
  case Value of
    #{R := _} ->
      add_required_errors(Required, Value, Path, ErrorMap);
    _ ->
    P = get_path(Path ++ [R]),
    add_required_errors(Required, Value, Path, ErrorMap#{P => [required | maps:get(P, ErrorMap, [])]})
  end;
add_required_errors([], _Value, _Path, ErrorMap) ->
  ErrorMap.

-spec get_path([any()]) -> any().
get_path([PathPart | PathParts]) ->
  get_path(PathParts, PathPart);
get_path([]) ->
  <<"root">>.

-spec get_path([atom() | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | integer()],_) -> any().
get_path([PathPart | PathParts], Res) ->
  get_path(PathParts, <<Res/binary, "__", (t2b(PathPart))/binary>>);
get_path([], Res) ->
  Res.


validate_values(ReqMap, Map) ->
  WrongValueList = maps:to_list(ReqMap) -- maps:to_list(Map),
  case WrongValueList of
    [] ->
      ok;
    _ ->
      {error, format_faulty_values(WrongValueList, Map)}
  end.

format_faulty_values(ReqList, Map) ->
  maps:from_list(lists:map(
    fun({ReqK, ReqV}) -> {ReqK , #{ req => ReqV, value => maps:get(ReqK, Map)}}
    end, ReqList)).


-spec t2b(atom() | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []) | integer()) -> binary().
t2b(V) when is_integer(V) -> integer_to_binary(V);
t2b(V) when is_list(V) -> list_to_binary(V);
t2b(V) when is_atom(V) -> atom_to_binary(V, utf8);
t2b(V) when is_binary(V) -> V.