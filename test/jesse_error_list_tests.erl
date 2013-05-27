%%%-----------------------------------------------------------------------------
%% @doc Set of unit tests for error accumulator feature of json schema
%%      validator.
%%
%% The basic tests below assume that the core validation functionality is
%% correct (draft3 test suite passes) and only check for the error collection
%% feature. Note also that the test results are checked for order, which depends
%% on the code of {@link jesse_schema_validator}.
%%
%% @since 5/27/13
%% @author Andrey Latnikov <alatnikov@alertlogic.com>
%%%-----------------------------------------------------------------------------
-module(jesse_error_list_tests).
-author("alatnikov").

-include_lib("eunit/include/eunit.hrl").

schema() ->
    [   {<<"type">>, <<"object">>},
        {<<"properties">>, [
            {<<"string">>, [
                {<<"type">>, <<"string">>}
            ]},

            {<<"contents">>, [
                {<<"type">>, <<"object">>},
                {<<"required">>, true},
                {<<"additionalProperties">>, false},
                {<<"properties">>, [
                    {<<"string">>, [
                        {<<"type">>, <<"string">>}
                    ]},
                    {<<"integer">>, [
                        {"<<type>>", "integer"}
                    ]},
                    {<<"array">>, [
                        {<<"type">>, <<"array">>},
                        {<<"required">>, true},
                        {<<"additionalItems">>, false},
                        {<<"minItems">>, 1},
                        {<<"uniqueItems">>, true},
                        {<<"items">>, [
                            {<<"type">>, <<"integer">>}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ].

valid_test() ->
    ValidExample = [
        {<<"string">>, <<"example-string">>},
        {<<"contents">>, [
            {<<"string">>, <<"another-string">>},
            {<<"integer">>, 10},
            {<<"array">>, [1,2,3]}
        ]}
    ],
    ?assertEqual({ok, ValidExample},
                 jesse:validate_with_accumulator(schema(), ValidExample)).

single_error_test() ->
    SingleError = [
        {<<"string">>, 121},
        {<<"contents">>, [
            {<<"string">>, <<"another-string">>},
            {<<"integer">>, 10},
            {<<"array">>, [1,2,3]}
        ]}
    ],
    ?assertMatch({error, [{data_invalid, _, not_string, _}]},
                 jesse:validate_with_accumulator(schema(), SingleError)).

double_error_test() ->
    DoubleError = [
        {<<"string">>, 121},
        {<<"contents">>, [
            {<<"string">>, <<"another-string">>},
            {<<"integer">>, 10}
        ]}
    ],
    ?assertMatch({error, [ {data_invalid, _, missing_required_property, _},
                           {data_invalid, _, not_string, _} ]},
                 jesse:validate_with_accumulator(schema(), DoubleError)).

triple_error_test() ->
    TripleError = [
        {<<"string">>, 121},
        {<<"contents">>, [
            {<<"string">>, <<"another-string">>},
            {<<"integer">>, <<"again-string">>},
            {<<"wrongfield">>, <<"wrong-value">>}
        ]}
    ],
    ?assertMatch({error, [ {data_invalid, _, missing_required_property, _},
                           {data_invalid, _, no_extra_properties_allowed, _},
                           {data_invalid, _, not_string, _} ]},
                 jesse:validate_with_accumulator(schema(), TripleError)).
