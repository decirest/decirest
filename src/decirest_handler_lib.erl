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
  persist_data/3,
  options/2,
  options_default/2,
  add_default_allow_header/2
]).

-spec validate_payload(binary(), map(), #{'module':=atom(), 'module_binding':=_, _=>_}) -> any().
validate_payload(Body, Req, State = #{module := Module}) ->
  case erlang:function_exported(Module, validate_payload, 3) of
    true ->
      Module:validate_payload(Body, Req, State);
    false ->
      Module:validate_payload(Body, State)
  end.

-spec persist_data(binary(), map(), #{'module':=atom(), _=>_}) -> any().
persist_data(Body, Req, State = #{module := Module}) ->
  case erlang:function_exported(Module, persist_data, 3) of
    true ->
      Module:persist_data(Body, Req, State);
    false ->
      Module:persist_data(Body, State)
  end.

options(Req, State = #{module := Module}) ->
  case decirest:do_callback(Module, options, Req, State, fun options_default/2) of
    no_call ->
      no_call;
    {ok, Req1, State1} ->
      case cowboy_req:has_resp_header(<<"allow">>, Req1) of
        true ->
          {ok, Req1, State1};
        false ->
          add_default_allow_header(Req1, State1)
      end
  end.

options_default(_Req, _State) ->
  %% Use cowboys internal return to indicate that the function
  %% is not exported and cowboy should use its default options response
  no_call.

add_default_allow_header(Req, #{allowed_methods := Methods} = State) ->
  <<", ", Allow/binary>> = <<<<", ", M/binary>> || M <- Methods>>,
  {ok, cowboy_req:set_resp_header(<<"allow">>, Allow, Req), State}.