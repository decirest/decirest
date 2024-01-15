%%%-------------------------------------------------------------------
%%% @author jso
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2017 09:47
%%%-------------------------------------------------------------------
-module(decirest_validator_tests).

-author("jso").

-include_lib("eunit/include/eunit.hrl").

simple_test() -> ?assert(true).

missing_params_test() ->
    Schema =
        #{<<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
          <<"properties">> =>
              #{<<"field1">> => #{<<"type">> => <<"string">>},
                <<"field2">> => #{<<"type">> => <<"integer">>}},
          <<"required">> => [<<"field1">>, <<"field2">>],
          <<"type">> => <<"object">>},
    ExpectedResult = {error, #{<<"field1">> => [required]}},
    Result = decirest_validator:validate_on_schema(#{<<"field2">> => 5}, Schema),
    ?assertEqual(ExpectedResult, Result).

deep_missing_params_test() ->
    Schema =
        #{<<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
          <<"properties">> =>
              #{<<"obj">> =>
                    #{<<"type">> => <<"object">>,
                      <<"properties">> => #{<<"f1">> => #{<<"type">> => <<"integer">>}},
                      <<"required">> => [<<"f1">>]},
                <<"field2">> => #{<<"type">> => <<"integer">>}},
          <<"required">> => [<<"obj">>, <<"field2">>],
          <<"type">> => <<"object">>},
    ExpectedResult = {error, #{<<"obj__f1">> => [required]}},
    Result =
        decirest_validator:validate_on_schema(#{<<"field2">> => 5,
                                                <<"obj">> => #{}},
                                              Schema),
    ?assertEqual(ExpectedResult, Result).

drop_test() ->
    Schema =
        #{<<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
          <<"additionalProperties">> => false,
          <<"properties">> =>
              #{<<"field1">> => #{<<"type">> => <<"string">>},
                <<"field2">> => #{<<"type">> => <<"integer">>}},
          <<"type">> => <<"object">>},
    ExpectedResult = {ok, #{<<"field2">> => 5}},
    Result =
        decirest_validator:validate_on_schema(#{<<"field2">> => 5,
                                                <<"not_walcome">> => 22},
                                              Schema),
    ?assertEqual(ExpectedResult, Result).

validate_values_test() ->
    Req = #{company => 1},
    ?assertEqual(ok, decirest_validator:validate_values(Req, #{company => 1})),
    ?assertEqual(ok,
                 decirest_validator:validate_values(Req,
                                                    #{company => 1,
                                                      qqq => 123})),
    ?assertEqual({error,
                  #{company =>
                        #{req => 1,
                          value => 2}}},
                 decirest_validator:validate_values(Req, #{company => 2})),
    ?assertEqual({error,
                  #{company =>
                        #{req => 1,
                          value => 2},
                    id =>
                        #{req => 33,
                          value => 22}}},
                 decirest_validator:validate_values(Req#{id => 33},
                                                    #{company => 2,
                                                      id => 22})).
