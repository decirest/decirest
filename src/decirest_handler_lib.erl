%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2018 08:18
%%%-------------------------------------------------------------------
-module(decirest_handler_lib).
-author("mikael").

%% API
-export([
  validate_payload/3,
  persist_data/3
]).

-spec validate_payload(binary(),map(),#{'module':=atom(), 'module_binding':=_, _=>_}) -> any().
validate_payload(Body, Req, State = #{module := Module}) ->
  case erlang:function_exported(Module, validate_payload, 3) of
    true ->
      Module:validate_payload(Body, Req, State);
    false ->
      Module:validate_payload(Body, State)
  end.

-spec persist_data(binary(),map(),#{'module':=atom(), _=>_}) -> any().
persist_data(Body, Req, State = #{module := Module}) ->
  case erlang:function_exported(Module, persist_data, 3) of
    true ->
      Module:persist_data(Body, Req, State);
    false ->
      Module:persist_data(Body, State)
  end.