-module(decirest_validator).
-export([
  validate_on_schema/2, validate_on_schema/3
]).

validate_on_schema(Obj, Schema) ->
  validate_on_schema(Obj, Schema, #{strict => false}). % false during test only

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


drop_extra_properties([Path | Paths], Obj) ->
  drop_extra_properties(Paths, drop_extra_property(Path, Obj));
drop_extra_properties([], Obj) ->
  Obj.

drop_extra_property([PathPart | []], Obj) ->
  maps:without([PathPart], Obj);
drop_extra_property([PathPart | PathParts], Obj) ->
  Obj#{PathPart => drop_extra_property(PathParts, maps:get(PathPart, Obj))}.


prettify_errors(Errors) ->
  prettify_errors(Errors, #{}).

prettify_errors([{data_invalid, _Schema, missing_required_property, Value, Path} | Errors], ErrorMap) ->
  P = get_path(Path ++ [Value]),
  prettify_errors(Errors, ErrorMap#{P => [<<"required">> | maps:get(P, ErrorMap, [])]});
prettify_errors([{data_invalid, _Schema, ErrorType, Value, Path} | Errors], ErrorMap) ->
  P = get_path(Path),
  prettify_errors(Errors, ErrorMap#{P => [[ErrorType, Value] | maps:get(P, ErrorMap, [])]});
prettify_errors([], ErrorMap) ->
  ErrorMap.


get_path([PathPart | PathParts]) ->
  get_path(PathParts, PathPart);
get_path([]) ->
  <<"root">>.

get_path([PathPart | PathParts], Res) ->
  get_path(PathParts, <<Res/binary, "__", (t2b(PathPart))/binary>>);
get_path([], Res) ->
  Res.

t2b(V) when is_integer(V) -> integer_to_binary(V);
t2b(V) when is_list(V) -> list_to_binary(V);
t2b(V) when is_atom(V) -> atom_to_binary(V, utf8);
t2b(V) when is_binary(V) -> V.
