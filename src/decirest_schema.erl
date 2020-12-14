%%%-------------------------------------------------------------------
%%% @author jonathan
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : 26. Oct 2020 20:14
%%%-------------------------------------------------------------------
-module(decirest_schema).
-author("jonathan").

%% API
-export([schema_to_md/1]).

schema_to_md(Json) ->
  [[<<"Schema\n---\n">>, pretty_prop({undefined, Json}, #{level => 0, required => []})]].

pretty_prop({Property, Json}, #{level := Level, required := Required}) ->
  {Indentation, ListSymbol} = style_level(Level),
  Default =  maps:get(<<"default">>, Json, undefined),
  Type = maps:get(<<"type">>, Json, <<"object">>),
  RequiredProps = get_required_properties(Type, Json),
  Properties = get_properties(Type, Json),
  List = maps:get(<<"enum">>, Json, []),

  DescriptionMd = get_description_md(maps:get(<<"description">>, Json, undefined), Indentation),
  TitleMd = get_title_md(Property, Json, Required),
  TypeMd = [<<"\n">>, Indentation, <<"*type*: ">>, Type, <<"\n">>],
  DefaultMd = get_default_md(Default, Indentation),

  PropertiesMd = [[Indentation, ListSymbol, pretty_prop(P, #{level => Level+1, required => RequiredProps})] || P <- maps:to_list(Properties)],
  ListMd = get_list_md(List, Indentation, ListSymbol),

  [<<"<box>">>, TitleMd, DescriptionMd, TypeMd, DefaultMd, ListMd, PropertiesMd, <<"</box>">>, <<"\n">>].

style_level(0) ->
  {<<"">>, <<"+ ">>};
style_level(1) ->
  {<<"    ">>, <<"* ">>};
style_level(2) ->
  {<<"        ">>, <<"- ">>}.

get_required_properties(<<"array">>, Json) ->
  maps:get(<<"required">>, maps:get(<<"items">>, Json, #{}), []);
get_required_properties(_, Json) ->
  maps:get(<<"required">>, Json).

get_properties(<<"array">>, Json) ->
  maps:get(<<"properties">>, maps:get(<<"items">>, Json, #{}), #{});
get_properties(_, Json) ->
  maps:get(<<"properties">>, Json, #{}).

get_title_md(undefined, Json, Required) ->
  % assume top-level, get "title" instead
  get_title_md(maps:get(<<"title">>, Json, <<"">>), Json, Required);
get_title_md(Property, _Json, Required) ->
  case lists:member(Property, Required) of
    false ->
      [<<"**">>, Property, <<"**">>, <<"\n">>];
    true ->
      [<<"**">>, Property, <<"** ">>, <<" *<required>required</required>*">>, <<"\n">>]
  end.

get_description_md(undefined, _) ->
  [];
get_description_md(Description, Indentation) ->
  [<<"\n">>, Indentation, Description, <<"\n">>].

get_list_md(List, Indentation, ListSymbol) ->
  [[Indentation, ListSymbol, E, <<"\n">>] || E <- List].

get_default_md(undefined, Indentation) ->
  [];
get_default_md(#{<<"element">> := Map}, Indentation) ->
  [];
%get_default_md(Map, Indentation);
get_default_md(Any, Indentation) ->
  Default = jiffy:encode(Any, [pretty]),
  [<<"\n">>, Indentation, <<"*default*: ">>, Default, <<"\n">>].
