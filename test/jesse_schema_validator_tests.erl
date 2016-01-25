%%%=============================================================================
%% Copyright 2014 Klarna AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc EUnit tests for jesse_schema_validator.
%% @end
%%%=============================================================================

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

  %% Object, additionalProperties, level 1
  Schema2 = {[
    {<<"type">>, <<"object">>},
    {<<"properties">>, {[
      {<<"foo">>, IntegerSchema}
    ]}},
    {<<"additionalProperties">>, false}
  ]},

  %% additionalProperties, level1
  ?assertThrow(
    [{data_invalid, Schema2, no_extra_properties_allowed, _, [<<"bar">>]}],
    jesse_schema_validator:validate(Schema2, {[{<<"foo">>, 0},
                                               {<<"bar">>, <<"baz">>}]}, [])
  ),

  %% Object, additionalProperties, level 2
  Schema3 = {[
    {<<"type">>, <<"object">>},
    {<<"properties">>, {[
      {<<"foo">>, {[
        {<<"type">>, <<"object">>},
        {<<"properties">>, {[
            {<<"subfoo">>, IntegerSchema}
        ]}},
        {<<"additionalProperties">>, false}
      ]}}
    ]}},
    {<<"additionalProperties">>, false}
  ]},

  %% additionalProperties, level 2
  ?assertThrow(
    [{data_invalid, _, no_extra_properties_allowed, _, [<<"foo">>, <<"bar">>]}],
    jesse_schema_validator:validate(Schema3,
                                    {[{<<"foo">>, {[
                                        {<<"subfoo">>, 1},
                                        {<<"bar">>, 2}
                                     ]}}]}, [])
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
                   , {[ {<<"3.4.5.6.7">>, {[{<<"type">>, <<"string">>}]}}
                      , {<<"additionalProperties">>, false}
                      ]}
                   }]},
  ValidJson   = {[{<<"3.4.5.6.7">>, <<"Hello world!">>}]},
  InvalidJson = {[{<<"3.4.5.6.7">>, true}]},

  ?assertEqual( {ok, ValidJson}
              , jesse_schema_validator:validate(Schema, ValidJson, [])
              ),
  ?assertThrow([{data_invalid,{[{<<"type">>,<<"string">>}]}, wrong_type, true, [<<"3.4.5.6.7">>]}]
              , jesse_schema_validator:validate(Schema, InvalidJson, [])
              ).

empty_list_as_valid_value_for_string_test() ->
  StringSchema = {[{<<"type">>, <<"string">>}]},

  EmptyListSchema = {[ {<<"type">>, <<"object">>}
                     , {<<"properties">>
                       , {[{<<"foo">>, StringSchema}]}}
                     ]},
  ?assertThrow(
     [{data_invalid, StringSchema, wrong_type, [], [<<"foo">>]}],
     jesse_schema_validator:validate(EmptyListSchema, [{<<"foo">>, []}], [])
    ).

schema_unsupported_test() ->
  SupportedSchema = {[{<<"$schema">>, <<"http://json-schema.org/draft-03/schema#">>}]},
  UnsupportedSchema = {[{<<"$schema">>, <<"http://json-schema.org/draft-05/schema#">>}]},

  Json = {[{<<"Doesn't matter">>}]},
  ?assertEqual( {ok, Json}
              , jesse_schema_validator:validate(SupportedSchema, Json, [])
              ),
  ?assertThrow([{schema_invalid, UnsupportedSchema,
                 {schema_unsupported, <<"http://json-schema.org/draft-05/schema#">>}}]
              , jesse_schema_validator:validate(UnsupportedSchema, Json, [])
              ).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
