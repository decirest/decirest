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

%% Types
-type http_status() :: cowboy:http_status().
-type req() :: cowboy_req:req().
-type http_headers() :: cowboy:http_headers().
-type resp_body() :: cowboy_req:resp_body().


-spec reply(http_status(), Req) -> Req when Req::req().
reply(Status, Req) ->
  cowboy_req:reply(Status, Req).

-spec reply(http_status(), http_headers(), resp_body(), Req) -> Req when Req::req().
reply(Status, Header, Body, Req) ->
  cowboy_req:reply(Status, Header, Body, Req).
