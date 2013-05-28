%%%-----------------------------------------------------------------------------
%% @doc Set of unit tests for error accumulator feature of json schema
%%      validator.
%%
%% @since 5/27/13
%% @author Andrey Latnikov <alatnikov@alertlogic.com>
%%%-----------------------------------------------------------------------------
-module(jesse_error_list_tests).
-author("alatnikov").

-include_lib("eunit/include/eunit.hrl").

-record(data, {title, string, integer, array}).

schema() ->
    [   {<<"type">>, <<"object">>},
        {<<"properties">>, [
            {<<"title">>, [
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
                        {<<"type">>, <<"integer">>}
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

to_json(#data{title = Title,
              string = String,
              integer = Integer,
              array = Array}) ->
    [
        {<<"title">>, Title},
        {<<"contents">>, [
            {<<"string">>, String},
            {<<"integer">>, Integer},
            {<<"array">>, Array}
        ]}
    ].

no_error() ->
    #data{title = <<"Title">>,
          string = <<"Valid string example">>,
          integer = 10,
          array = [1,2,3,4]}.

single_error() ->
    (no_error())#data{integer = <<"Wrong type">>}.

double_error() ->
    (single_error())#data{array = [1, 2, 3, 1]}.

triple_error() ->
    (double_error())#data{string = [ {<<"unexpected">>, <<"field">>} ]}.

valid_test() ->
    Valid = to_json(no_error()),
    ?assertEqual({ok, Valid}, jesse:validate_with_accumulator(schema(), Valid)).

single_error_test() ->
    check_errors(1, single_error()).

double_error_test() ->
    check_errors(2, double_error()).

triple_error_test() ->
    check_errors(3, triple_error()).

custom_accumulator_test() ->
    SingleError = to_json(single_error()),
    MyFun = fun (Error, my) -> [{my_data, Error}] end,
    ?assertMatch({error, [ {my_data, _} ]},
        jesse:validate_with_accumulator(schema(), SingleError, MyFun, my)).

check_errors(Num, Data) ->
    {error, Errors} = jesse:validate_with_accumulator(schema(), to_json(Data)),
    ?assert(is_list(Errors)),
    ?assertEqual(Num, length(Errors)),
    ?assertEqual(ok, lists:foreach(fun match_error/1, Errors)).

match_error({'data_invalid', _, Type, _}) -> match_error_type(Type);
match_error({'schema_invalid', _, Type}) -> match_error_type(Type);
match_error(Error) -> throw({'wrong_error_format', Error}).

match_error_type(Atom) when is_atom(Atom) -> ok;
match_error_type(Tuple) when is_tuple(Tuple)
    andalso is_atom(element(1, Tuple)) -> ok;
match_error_type(Type) -> throw({'wrong_error_type', Type}).