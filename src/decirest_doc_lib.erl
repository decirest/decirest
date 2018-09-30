%%%-------------------------------------------------------------------
%%% @author jso
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2018 21:42
%%%-------------------------------------------------------------------
-module(decirest_doc_lib).
-author("jso").

%% API
-export([
  fetch_doc_by_module/4,
  fetch_doc_by_path/3,
  get_all_model_routes/1,
  get_model_routes/2
]).

fetch_doc_by_module(Ref, Module, Req, State) ->
  lager:critical("by module"),
  case get_all_model_routes(Ref) of
    #{Module := [FirstPath | _]} ->
      fetch_doc_by_path(Ref, Req#{path => FirstPath}, State);
    _ ->
      stop
  end.

fetch_doc_by_path(Ref, Req, State) ->
  %lager:critical("by path, ~p", [Req]),
  #{env := Env} = ranch:get_protocol_options(Ref),
  case cowboy_router:execute(Req, Env) of
    {ok, _ReqNew, _EnvNew = #{handler := H, handler_opts := HO}} ->
      %lager:critical("found it, going forward, H = ~p, HO = ~p", [H, HO]),
      make_doc_map(Ref, H, HO, Req, State);
    Res ->
      lager:critical("stop, ~p", [Res]),
      stop
  end.


make_doc_map(Ref, Handler, HandlerOpts = #{main_module := Module}, Req, State) ->
  D0 = #{name => atom_to_binary(Module, utf8), module => Module},
  D1 = maps:merge(D0, fetch_info(Handler, HandlerOpts, Req, State)),
  D2 = maps:merge(D1, decirest:apply_with_default(Module, doc, [], #{})),
  Paths = get_model_routes(Ref, Module),
  D2#{paths => Paths}.

fetch_info(Handler, HandlerOpts, Req, State0 = #{module := Module, child_fun := ChildFun}) ->
  State = maps:merge(State0, HandlerOpts),
  {AM, _, _} = Handler:allowed_methods(Req, State),
  {CTA, _, _} = Handler:content_types_accepted(Req, State),
  {CTP, _, _} = Handler:content_types_provided(Req, State),
  ChildUrls = decirest:child_urls_map(ChildFun(Module), Req, State),
  #{
    allowed_methods => AM,
    content_types_allowed => fix_content_type(CTA),
    content_types_provided => fix_content_type(CTP),
    children => ChildUrls
  }.

get_all_model_routes(Ref) ->
  % we assume just one host definition for now, this needs to be relaxed
  #{env := #{dispatch := [{_, _, Paths}]}} = ranch:get_protocol_options(Ref),
  PS = [{make_presentation_path(P), M} || {P, _, _, #{main_module := M}} <- Paths],
  lists:foldl(fun resource_path_fold/2, #{}, lists:reverse(PS)).

get_model_routes(Ref, Module) ->
  % we assume just one host definition for now, this needs to be relaxed
  #{env := #{dispatch := [{_, _, Paths}]}} = ranch:get_protocol_options(Ref),
  [make_presentation_path(P) || {P, _, _, #{main_module := M}} <- Paths, M == Module].
  % case get_all_model_routes(Ref) of
    % #{Module := Routes} -> Routes;
    % _ -> notfound
  % end.

fix_content_type(List) ->
  [<<A/binary, "/", B/binary>> || {{A, B, _}, _} <- List].

make_presentation_path(Path) ->
  make_presentation_path(Path,<<"/">>).

make_presentation_path([P | Path], Pth) when is_atom(P) ->
  make_presentation_path(Path, <<Pth/binary, ":", (atom_to_binary(P, utf8))/binary, "/">>);
make_presentation_path([P | Path], Pth) when is_binary(P) ->
  make_presentation_path(Path, <<Pth/binary, P/binary, "/">>);
make_presentation_path([], Pth) ->
  Pth.

resource_path_fold({Path, Module}, Acc) ->
  case Acc of
    #{Module := Paths} ->
      Acc#{Module => [Path | Paths]};
    _ ->
      Acc#{Module => [Path]}
  end.

