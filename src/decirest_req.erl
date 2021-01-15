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
  binding/2,
  bindings/1,
  header/2,
  host/1,
  method/1,
  parse_cookies/1,
  path/1,
  reply/2,
  reply/4,
  set_resp_body/2,
  set_resp_header/3,
  uri/1
]).

%% Types
-type http_status() :: cowboy:http_status().
-type req() :: cowboy_req:req().
-type http_headers() :: cowboy:http_headers().
-type resp_body() :: cowboy_req:resp_body().

-export_type([req/0]).


-spec binding(atom(), req()) -> any() | undefined.
binding(Name, Req) ->
  cowboy_req:binding(Name, Req).

-spec bindings(req()) -> cowboy_router:bindings().
bindings(Req) ->
  cowboy_req:bindings(Req).

-spec header(binary(), req()) -> binary() | undefined.
header(Name, Req) ->
  cowboy_req:header(Name, Req).

-spec host(req()) -> binary().
host(Req) ->
  cowboy_req:host(Req).

-spec method(req()) -> binary().
method(Req) ->
  cowboy_req:method(Req).

-spec parse_cookies(req()) -> [{binary(), binary()}].
parse_cookies(Req) ->
  cowboy_req:parse_cookies(Req).

-spec path(req()) -> binary().
path(Req) ->
  cowboy_req:path(Req).

-spec reply(http_status(), Req) -> Req when Req::req().
reply(Status, Req) ->
  cowboy_req:reply(Status, Req).

-spec reply(http_status(), http_headers(), resp_body(), Req) -> Req when Req::req().
reply(Status, Header, Body, Req) ->
  cowboy_req:reply(Status, Header, Body, Req).

-spec set_resp_body(resp_body(), Req) -> Req when Req::req().
set_resp_body(Body, Req) ->
  cowboy_req:set_resp_body(Body, Req).

-spec set_resp_header(binary(), iodata(), Req)
      -> Req when Req::req().
set_resp_header(Name, Value, Req) ->
  cowboy_req:set_resp_header(Name, Value, Req).

-spec uri(req()) -> iodata().
uri(Req) ->
  cowboy_req:uri(Req).
