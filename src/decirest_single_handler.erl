-module(decirest_single_handler).
-export([
  init/2,
  content_types_provided/2,
  get_html/2,
  resource_exists/2
]).

init(Req, State) ->
  io:format("~p~n", [Req]),
  {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
  {<<"<html><body>", (jsx:encode(maps:without([child_fun], State), [indent]))/binary, "</body></html>">>, Req, State}.

resource_exists(Req, State) ->
  {Res, ReqNew, StateNew} = Log = decirest:call_mro(resource_exists, Req, State, true, fun(true) -> true;(_) -> false end),
  io:format("Resource Exist result ~p~n", [State]),
  {true, Req, State}.

single_exists(RD, Ctx = #context{imaparent = ImAParent, submodule = Mod, rstate = RState}) ->
  case {ImAParent, wrq:path_info(Mod:ident(), RD)} of
    {true, undefined} ->
      lager:info("I'm a unused parent ~p~n", [Mod]),
      {true, RD, Ctx};
    {_, ID} ->
      lager:info("single resource ~p has value ~p in path~n", [Mod, ID]),
      case Mod:get_raw_data(wrq:path_info(RD)) of
        {ok, Data} ->
          {true, RD, Ctx#context{rstate = RState#{Mod => #{data => Data}}}};
        {error, Reason} ->
          lager:info("I don't exists, ~p, Reason = ~p~n", [Mod, Reason]),
          case {ImAParent, wrq:method(RD)} of
            {false, 'PUT'} ->
              lager:info("I'm the actuall called resource and we're doing a put, continue even if I don't exist"),
              {true, RD, Ctx};
            _ ->
              {false, RD, Ctx}
          end
      end
  end.
