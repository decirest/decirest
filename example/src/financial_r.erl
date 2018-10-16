-module(financial_r).

-export([
  name/0,
  paths/0,
  child_of/0,
  fetch_data/2,
  forbidden/2
]).

name() -> <<"financial">>.
paths() -> [{"/financial", decirest_single_handler}].

child_of() -> [account_r].

fetch_data(_, _) -> {ok, [#{<<"something">> => <<"clever">>}]}.

forbidden(Req, State) ->
  % this resources is only allowed in the back office
  case decirest:is_ansestor(bo_r, State) of
    true ->
      lager:critical("~n~n allow it ~p~n~n", [State]),
      {false, Req, State};
    false ->
      lager:critical("~n~n disallow it ~p~n~n", [State]),
      {true, Req, State}
  end.
