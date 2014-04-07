%% @doc EUnit tests for jesse_schema_validator.
-module(jesse_schema_validator_tests).
-include_lib("eunit/include/eunit.hrl").

data_invalid_test() ->
  IntegerSchema = {[{<<"type">>, <<"integer">>}]},

  %% A case without errors
  ?assertEqual(
    {ok, 42},
    jesse_schema_validator:validate(IntegerSchema, 42, [])
  ),

  %% A schema for testing properties and patternProperties
  Schema = {[
    {<<"type">>, <<"object">>},
    {<<"properties">>, {[
      {<<"foo">>, {[
        {<<"type">>, <<"object">>},
        {<<"properties">>, {[
            {<<"subfoo">>, IntegerSchema}
        ]}}
      ]}}
    ]}},
    {<<"patternProperties">>, {[
      {<<"^b">>, IntegerSchema}
    ]}}
  ]},

  %% Root level error
  ?assertThrow(
    [{data_invalid, Schema, wrong_type, <<"foo">>, []}],
    jesse_schema_validator:validate(Schema, <<"foo">>, [])
  ),

  %% Properties, 2 levels
  ?assertThrow(
    [{data_invalid, IntegerSchema, wrong_type, <<"bar">>, [<<"foo">>, <<"subfoo">>]}],
    jesse_schema_validator:validate(Schema, {[{<<"foo">>, {[{<<"subfoo">>, <<"bar">>}]}}]}, [])
  ),

  %% patternProperties, level 1
  ?assertThrow(
    [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [<<"bar">>]}],
    jesse_schema_validator:validate(Schema, {[{<<"bar">>, <<"baz">>}]}, [])
  ),

  %% Items: A zero-based index is used in the property path
  ItemsSchema = {[
    {<<"type">>, <<"array">>},
    {<<"items">>, IntegerSchema},
    {<<"maxItems">>, 3}
  ]},
  ?assertThrow(
    [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [1]}],
    jesse_schema_validator:validate(ItemsSchema, [2, <<"baz">>, 3], [])
  ),
  ?assertThrow(
    [{data_invalid, ItemsSchema, wrong_size, [2, 3, 4, 5], []}],
    jesse_schema_validator:validate(ItemsSchema, [2, 3, 4, 5], [])
  ),

  %% Items, a schema per item
  ItemsSchema2 = {[
    {<<"type">>, <<"array">>},
    {<<"items">>, [IntegerSchema, IntegerSchema, IntegerSchema]},
    {<<"additionalItems">>, false}
  ]},
  ?assertThrow(
    [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [2]}],
    jesse_schema_validator:validate(ItemsSchema2, [2, 3, <<"baz">>], [])
  ),
  ?assertThrow(
    [{data_invalid, ItemsSchema2, no_extra_items_allowed, [2, 3, 4, 5], []}],
    jesse_schema_validator:validate(ItemsSchema2, [2, 3, 4, 5], [])
  ),

  %% Dependencies
  DependenciesSchema = {[
    {<<"type">>, <<"object">>},
    {<<"dependencies">>, {[
      {<<"bar">>, [<<"foo">>]} %% if there is bar, there must also be foo
    ]}}
  ]},
  ?assertThrow(
    [{data_invalid, DependenciesSchema, {missing_dependency, <<"foo">>}, {[{<<"bar">>, 42}]}, []}],
    jesse_schema_validator:validate(DependenciesSchema, {[{<<"bar">>, 42}]}, [])
  ),

  ok.

dots_used_in_keys_test() ->
  Schema      = {[ {<<"type">>, <<"object">>}
                 , {<<"properties">>
                   , {[{<<"3.4.5.6.7">>, {[{<<"type">>, <<"string">>}]}}]}
                   }]},
  ValidJson   = {[{<<"3.4.5.6.7">>, <<"Hello world!">>}]},
  InvalidJson = {[{<<"3.4.5.6.7">>, true}]},

  ?assertEqual( {ok, ValidJson}
              , jesse_schema_validator:validate(Schema, ValidJson, [])
              ),
  ?assertThrow([{data_invalid,{[{<<"type">>,<<"string">>}]}, wrong_type, true, [<<"3.4.5.6.7">>]}]
              , jesse_schema_validator:validate(Schema, InvalidJson, [])
              ).


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
