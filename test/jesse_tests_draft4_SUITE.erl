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
%%
%% @doc jesse test suite which covers Draft 04. It uses JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%% @end
%%%=============================================================================

-module(jesse_tests_draft4_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ additionalItems/1
        , additionalProperties/1
        , allOf/1
        , anyOf/1
        , default/1
        , definitions/1
        , dependencies/1
        , enum/1
        , items/1
        , maximum/1
        , maxItems/1
        , maxLength/1
        , maxProperties/1
        , minimum/1
        , minItems/1
        , minLength/1
        , minProperties/1
        , multipleOf/1
        , 'not'/1
        , oneOf/1
        , pattern/1
        , patternProperties/1
        , properties/1
        , ref/1
        , refRemote/1
        , required/1
        , type/1
        , uniqueItems/1
        ]).

-include_lib("common_test/include/ct.hrl").

-define(json_schema_draft4, <<"http://json-schema.org/draft-04/schema#">>).
-define(TESTS_DIR, "JSON-Schema-Test-Suite/tests/draft4").

%% JSON-Schema-Test-Suite attributes definitions
-define(DATA,        <<"data">>).
-define(DESCRIPTION, <<"description">>).
-define(SCHEMA,      <<"schema">>).
-define(TESTS,       <<"tests">>).
-define(VALID,       <<"valid">>).

all() ->
  [ additionalItems
  , additionalProperties
  , allOf
  , anyOf
  , default
  , definitions
  , dependencies
  , enum
  , items
  , maximum
  , maxItems
  , maxLength
  , maxProperties
  , minimum
  , minItems
  , minLength
  , minProperties
  , multipleOf
  , 'not'
  , oneOf
  , pattern
  , patternProperties
  , properties
  , ref
  , refRemote
  , required
  , type
  , uniqueItems
  ].

init_per_suite(Config) ->
  inets:start(),
  load_test_specs() ++ Config.

end_per_suite(_Config) ->
  inets:stop().

%%% Testcases
additionalItems(Config) ->
  do_test("additionalItems", Config).

additionalProperties(Config) ->
  do_test("additionalProperties", Config).

allOf(Config) ->
  do_test("allOf", Config).

anyOf(Config) ->
  do_test("anyOf", Config).

default(Config) ->
  do_test("default", Config).

definitions(Config) ->
  do_test("definitions", Config).

dependencies(Config) ->
  do_test("dependencies", Config).

enum(Config) ->
  do_test("enum", Config).

items(Config) ->
  do_test("items", Config).

maximum(Config) ->
  do_test("maximum", Config).

maxItems(Config) ->
  do_test("maxItems", Config).

maxLength(Config) ->
  do_test("maxLength", Config).

maxProperties(Config) ->
  do_test("maxProperties", Config).

minimum(Config) ->
  do_test("minimum", Config).

minItems(Config) ->
  do_test("minItems", Config).

minLength(Config) ->
  do_test("minLength", Config).

minProperties(Config) ->
  do_test("minProperties", Config).

multipleOf(Config) ->
  do_test("multipleOf", Config).

'not'(Config) ->
  do_test("not", Config).

oneOf(Config) ->
  do_test("oneOf", Config).

pattern(Config) ->
  do_test("pattern", Config).

patternProperties(Config) ->
  do_test("patternProperties", Config).

properties(Config) ->
  do_test("properties", Config).

ref(Config) ->
  do_test("ref", Config).

refRemote(Config) ->
  ServerOpts = [{port, 1234}, {server_name, "localhost"}, {server_root, "."},
                {document_root, "test/JSON-Schema-Test-Suite/remotes"},
                {bind_address, "localhost"}],
  inets:start(httpd, ServerOpts),
  do_test("refRemote", Config).

required(Config) ->
  do_test("required", Config).

type(Config) ->
  do_test("type", Config).

uniqueItems(Config) ->
  do_test("uniqueItems", Config).

%%% Internal functions
do_test(Key, Config) ->
    run_tests(?config(Key, Config)).

run_tests(Specs) ->
  lists:foreach( fun(Spec) ->
                     Description = get_path(?DESCRIPTION, Spec),
                     Schema      = get_path(?SCHEMA, Spec),
                     TestSet     = get_path(?TESTS, Spec),
                     ct:pal("** Test set: ~s~n** Schema: ~p~n",
                            [Description, Schema]),
                     run_test_set(Schema, TestSet)
                 end
               , Specs
               ).

run_test_set(Schema, TestSet) ->
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     TestData    = get_path(?DATA, Test),
                     ct:pal("* Test case: ~s~n", [Description]),
                     Opts = [{default_schema_ver, ?json_schema_draft4},
                             {schema_loader_fun, fun load_schema/1}],
                     try jesse:validate_with_schema(Schema, TestData, Opts) of
                         Result ->
                             ct:pal("Result: ~p~n", [Result]),
                             case get_path(?VALID, Test) of
                                 true  -> {ok, TestData} = Result;
                                 false -> {error, _} = Result
                             end
                     catch C:E ->
                               ct:pal("Error: ~p:~p~nStacktrace: ~p~n",
                                      [C, E, erlang:get_stacktrace()])
                     end
                 end
               , TestSet
               ).

load_test_specs() ->
  TestsDir = filename:join(os:getenv("TEST_DIR"), ?TESTS_DIR),
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

load_schema(URI) ->
  URIStr = unicode:characters_to_list(URI),
  case httpc:request(get, {URIStr, []}, [], []) of
    {ok, {{_Line, 200, _}, _Headers, Body}} -> jiffy:decode(Body);
    {error, no_scheme} -> load_schema(URI)
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
