%%%=============================================================================
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
%% @copyright 2013 Klarna AB
%% @author Alexander Dergachev <alexander.dergachev@klarna.com>
%%
%% @doc jesse test suite which covers Draft 03. It uses JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%% @end
%%%=============================================================================

-module(jesse_tests_draft3_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ additionalItems/1
        , additionalProperties/1
        , dependencies/1
        , disallow/1
        , divisibleBy/1
        , enum/1
        , extends/1
        , items/1
        , maximum/1
        , maxItems/1
        , maxLength/1
        , minimum/1
        , minItems/1
        , minLength/1
        , pattern/1
        , patternProperties/1
        , properties/1
        %% , ref/1
        , required/1
        , type/1
        , uniqueItems/1
        ]).

-include_lib("common_test/include/ct.hrl").

-define(TESTS_DIR, "JSON-Schema-Test-Suite/tests/draft3").

%% JSON-Schema-Test-Suite attributes definitions
-define(DATA,        <<"data">>).
-define(DESCRIPTION, <<"description">>).
-define(SCHEMA,      <<"schema">>).
-define(TESTS,       <<"tests">>).
-define(VALID,       <<"valid">>).

all() ->
  [ additionalItems
  , additionalProperties
  , dependencies
  , disallow
  , divisibleBy
  , enum
  , extends
  , items
  , maximum
  , maxItems
  , maxLength
  , minimum
  , minItems
  , minLength
  , pattern
  , patternProperties
  , properties
  %% , ref
  , required
  , type
  , uniqueItems
  ].

%%%
init_per_suite(Config) ->
  TestsDir  = filename:join(?config(data_dir, Config), ?TESTS_DIR),
  TestSpecs = load_test_specs(TestsDir),
  TestSpecs ++ Config.

end_per_suite(_Config) ->
  ok.

%%% Testcases
additionalItems(Config) ->
  Key   = "additionalItems",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

additionalProperties(Config) ->
  Key   = "additionalProperties",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

dependencies(Config) ->
  Key   = "dependencies",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

disallow(Config) ->
  Key   = "disallow",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

divisibleBy(Config) ->
  Key   = "divisibleBy",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

enum(Config) ->
  Key   = "enum",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

extends(Config) ->
  Key   = "extends",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

items(Config) ->
  Key   = "items",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

maximum(Config) ->
  Key   = "maximum",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

maxItems(Config) ->
  Key   = "maxItems",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

maxLength(Config) ->
  Key   = "maxLength",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

minimum(Config) ->
  Key   = "minimum",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

minItems(Config) ->
  Key   = "minItems",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

minLength(Config) ->
  Key   = "minLength",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

pattern(Config) ->
  Key   = "pattern",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

patternProperties(Config) ->
  Key   = "patternProperties",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

properties(Config) ->
  Key   = "properties",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

%% not implemented yet
%% ref(Config) ->
%%   Key   = "ref",
%%   Specs = ?config(Key, Config),
%%   ok    = run_tests(Key, Specs).

required(Config) ->
  Key   = "required",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

type(Config) ->
  Key   = "type",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

uniqueItems(Config) ->
  Key   = "uniqueItems",
  Specs = ?config(Key, Config),
  ok    = run_tests(Key, Specs).

%%% Internal functions
run_tests(Key, Specs) ->
  lists:foreach( fun(Spec) ->
                     Description = get_path(?DESCRIPTION, Spec),
                     Schema      = get_path(?SCHEMA, Spec),
                     TestSet     = get_path(?TESTS, Spec),
                     io:format("** Test set: ~s~n", [Description]),
                     run_test_set(Schema, TestSet)
                 end
               , Specs
               ).

run_test_set(Schema, TestSet) ->
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     TestData    = get_path(?DATA, Test),
                     io:format("* Test case: ~s~n", [Description]),
                     Result = jesse:validate_with_schema(Schema, TestData),
                     io:format("Result: ~p~n", [Result]),
                     case get_path(?VALID, Test) of
                       true  -> {ok, TestData} = Result;
                       false -> {error, _} = Result
                     end
                 end
               , TestSet
               ).

load_test_specs(TestsDir) ->
  FileList = filelib:wildcard(TestsDir ++ "/*.json"),
  lists:map( fun(Filename) ->
                 {ok, Bin} = file:read_file(Filename),
                 JsonTest  = jiffy:decode(Bin),
                 {filename_to_key(Filename), JsonTest}
             end
           , FileList
           ).

filename_to_key(Filename) ->
  filename:rootname(filename:basename(Filename)).

get_path(Key, Schema) ->
  jesse_json_path:path(Key, Schema).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
