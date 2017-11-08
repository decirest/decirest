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

simple_test() ->
  ?assert(true).

missing_params_test() ->
  Schema = #{
    <<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
    <<"id">> => <<"http://schema.decirest.com/schema/account.json">>,
    <<"properties">> => #{
      <<"field1">> => #{<<"type">> => <<"string">>},
      <<"field2">> => #{<<"type">> => <<"integer">>}
    },
    <<"required">> => [<<"field1">>,<<"field2">>],
    <<"type">> => <<"object">>
  },
  ExpectedResult = {error, #{<<"field1">> => [required]}},
  Result = decirest_validator:validate_on_schema(#{<<"field2">> => 5}, Schema),
  ?assertEqual(ExpectedResult, Result).

deep_missing_params_test() ->
  Schema = #{
    <<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
    <<"id">> => <<"http://schema.decirest.com/schema/account.json">>,
    <<"properties">> => #{
      <<"obj">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"f1">> => #{<<"type">> => <<"integer">>}
        },
        <<"required">> => [<<"f1">>]
      },
      <<"field2">> => #{<<"type">> => <<"integer">>}
    },
    <<"required">> => [<<"obj">>,<<"field2">>],
    <<"type">> => <<"object">>
  },
  ExpectedResult = {error, #{<<"obj__f1">> => [required]}},
  Result = decirest_validator:validate_on_schema(#{<<"field2">> => 5, <<"obj">> => #{}}, Schema),
  ?assertEqual(ExpectedResult, Result).
