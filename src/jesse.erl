%%%=============================================================================
%% Copyright 2013 Klarna AB
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
%% @copyright 2013 Klarna AB
%% @author Alexander Dergachev <alexander.dergachev@klarna.com>
%%
%% @doc JESSE (JSon Schema Erlang)
%%
%% This is an interface module which provides an access to the main
%% functionality of jesse, such as 1) updating of the schema definitions cache;
%% 2) validation json data against a schema.
%% @end
%%%=============================================================================

-module(jesse).

%% API
-export([ add_schema/2
        , add_schema/3
        , del_schema/1
        , load_schemas/2
        , load_schemas/4
        , validate/2
        , validate/3
        , validate_with_schema/2
        , validate_with_schema/3
        ]).

-export_type([ json_term/0
             ]).

-type json_term() :: term().
-type error()     :: {error, term()}.

%%% API
%% @doc Adds a schema definition `Schema' to in-memory storage associated with
%% a key `Key'. It will overwrite an existing schema with the same key if
%% there is any.
-spec add_schema(Key :: any(), Schema :: json_term()) -> ok | error().
add_schema(Key, Schema) ->
  ValidationFun = fun jesse_schema_validator:is_json_object/1,
  MakeKeyFun    = fun(_) -> Key end,
  jesse_database:add(Schema, ValidationFun, MakeKeyFun).

%% @doc Equivalent to `add_schema/2', but `Schema' is a binary string, and
%% the third agument is a parse function to convert the binary string to
%% a supported internal representation of json.
-spec add_schema( Key      :: any()
                , Schema   :: binary()
                , ParseFun :: fun((binary()) -> json_term())
                ) -> ok | error().
add_schema(Key, Schema, ParseFun) ->
  case try_parse(ParseFun, Schema) of
    {parse_error, _} = SError -> {error, {schema_error, SError}};
    ParsedSchema              -> add_schema(Key, ParsedSchema)
  end.


%% @doc Deletes a schema definition from in-memory storage associated with
%% the key `Key'.
-spec del_schema(Key :: any()) -> ok.
del_schema(Key) ->
  jesse_database:delete(Key).

%% @doc Loads schema definitions from filesystem to in-memory storage.
%%
%% Equivalent to `load_schemas(Path, ParseFun, ValidationFun, MakeKeyFun)'
%% where `ValidationFun' is `fun jesse_json:is_json_object/1' and
%% `MakeKeyFun' is `fun jesse_schema_validator:get_schema_id/1'. In this case
%% the key will be the value of `id' attribute from the given schemas.
-spec load_schemas( Path     :: string()
                  , ParseFun :: fun((binary()) -> json_term())
                  ) -> jesse_database:update_result().
load_schemas(Path, ParseFun) ->
  load_schemas( Path
              , ParseFun
              , fun jesse_schema_validator:is_json_object/1
              , fun jesse_schema_validator:get_schema_id/1
              ).

%% @doc Loads schema definitions from filesystem to in-memory storage.
%% The function loads all the files from directory `Path', then each schema
%% entry will be checked for a validity by function `ValidationFun', and
%% will be stored in in-memory storage with a key returned by `MakeKeyFun'
%% function.
%%
%% In addition to a schema definition, a timestamp of the schema file will be
%% stored, so, during the next update timestamps will be compared to avoid
%% unnecessary updates.
%%
%% Schema definitions are stored in the format which json parsing function
%% `ParseFun' returns.
%%
%% NOTE: it's impossible to automatically update schema definitions added by
%%       add_schema/2, the only way to update them is to use add_schema/2
%%       again with the new definition.
-spec load_schemas( Path          :: string()
                  , ParseFun      :: fun((binary()) -> json_term())
                  , ValidationFun :: fun((any()) -> boolean())
                  , MakeKeyFun    :: fun((json_term()) -> any())
                  ) -> jesse_database:update_result().
load_schemas(Path, ParseFun, ValidationFun, MakeKeyFun) ->
  jesse_database:update(Path, ParseFun, ValidationFun, MakeKeyFun).

%% @doc Validates json `Data' against a schema with the same key as `Schema'
%% in the internal storage. If the given json is valid, then it is returned
%% to the caller, otherwise an error with an appropriate error reason
%% is returned.
-spec validate(Schema :: any(), Data :: json_term()) -> {ok, json_term()}
                                                      | error().
validate(Schema, Data) ->
  try
    JsonSchema = jesse_database:read(Schema),
    jesse_schema_validator:validate(JsonSchema, Data)
  catch
    throw:Error ->
      {error, Error}
  end.

%% @doc Equivalent to `validate/2', but `Data' is a binary string, and
%% the third agument is a parse function to convert the binary string to
%% a supported internal representation of json.
-spec validate( Schema   :: any()
              , Data     :: binary()
              , ParseFun :: fun((binary()) -> json_term())
              ) -> {ok, json_term()}
                 | error().
validate(Schema, Data, ParseFun) ->
  case try_parse(ParseFun, Data) of
    {parse_error, _} = DError -> {error, {data_error, DError}};
    ParsedJson                -> validate(Schema, ParsedJson)
  end.

%% @doc Validates json `Data' agains the given schema `Schema'. If the given
%% json is valid, the it is returned to the caller, otherwise an error with
%% an appropriate error reason is returned.
-spec validate_with_schema( Schema :: json_term()
                          , Data   :: json_term()
                          ) -> {ok, json_term()}
                             | error().
validate_with_schema(Schema, Data) ->
  try
    jesse_schema_validator:validate(Schema, Data)
  catch
    throw:Error ->
      {error, Error}
  end.

%% @doc Equivalent to `validate_with_schema/2', but both `Schema' and
%% `Data' are binary strings, and the third arguments is a parse function
%% to convert the binary string to a supported internal representation of json.
-spec validate_with_schema( Schema   :: binary()
                          , Data     :: binary()
                          , ParseFun :: fun((binary()) -> json_term())
                          ) -> {ok, json_term()}
                             | error().
validate_with_schema(Schema, Data, ParseFun) ->
  case try_parse(ParseFun, Schema) of
    {parse_error, _} = SError ->
      {error, {schema_error, SError}};
    ParsedSchema ->
      case try_parse(ParseFun, Data) of
        {parse_error, _} = DError ->
          {error, {data_error, DError}};
        ParsedData ->
          validate_with_schema(ParsedSchema, ParsedData)
      end
  end.

%%% Internal functions
%% @doc Wraps up calls to a third party json parser.
%% @private
try_parse(ParseFun, JsonBin) ->
  try
    ParseFun(JsonBin)
  catch
    _:Error ->
      {parse_error, Error}
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
