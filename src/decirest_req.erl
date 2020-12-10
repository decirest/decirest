%%%-------------------------------------------------------------------
%%% @author alexei
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2020 3:49 PM
%%%-------------------------------------------------------------------
-module(decirest_req).
-author("alexei").

%% API
-export([
  reply/2,
  reply/4
]).


-spec reply(non_neg_integer(), map()) -> map() | error.
reply(Status, Req) ->
  cowboy_req:reply(Status, Req).

-spec reply(non_neg_integer(), map(), map(), map()) -> map() | error.
reply(Status, Header, Body, Req) ->
  cowboy_req:reply(Status, Header, Body, Req).
