%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2020, SIGICOM
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2020 21:56
%%%-------------------------------------------------------------------
-module(decirest_ws_handler).

-author("mikael").

%% API
-export([req/1,
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         resource_exists/2]).

  %% Needed to be able to work with other resources that
  %% use single and collection handler

req(#{inital_req := Req}) -> Req.

-spec init(_, map()) ->
              {cowboy_rest,
               _,
               #{rstate := #{},
                 _ => _}}.
init(Req, State = #{module := Module}) ->
    decirest:apply_with_default(Module, init, [Req, State], fun init_default/2).

init_default(Req, State) ->
    {cowboy_websocket,
     Req,
     State#{inital_req => Req,
            rstate => #{}}}.

websocket_init(State = #{module := Module}) ->
    decirest:apply_with_default(Module,
                                websocket_init,
                                [State],
                                fun websocket_init_default/1).

websocket_init_default(State0) ->
    case decirest_handler_lib:is_authorized(req(State0), State0) of
        {true, Req, State} ->
            {_Res, _ReqNew, StateNew} = decirest:call_mro(resource_exists, Req, State, true),
            {[], StateNew};
        {false, _Req, State} -> {[], State}
    end.

websocket_handle(Msg, #{module := Module} = State) ->
    case decode_message(Msg) of
        {ok, DecodedMsg} ->
            case Module:websocket_handle(DecodedMsg, State) of
                {ok, State} -> {ok, State};
                {reply, Res, NewState} -> {[{binary, term_to_binary(Res, [compressed])}], NewState}
            end;
        {error, unable_to_decode} -> {[{text, <<"unable_to_decode">>}], State}
    end.

websocket_info({'EXIT', _Pid, _Reason}, _State) -> {[], ok}.

decode_message({text, Txt}) -> {ok, Txt};
decode_message({binary, Binary}) -> decode_message(Binary);
decode_message(Msg) when is_binary(Msg) ->
    try binary_to_term(Msg) of Term -> {ok, Term} catch _:_ -> {error, unable_to_decode} end.

resource_exists(Req, State) -> {true, Req, State}.
