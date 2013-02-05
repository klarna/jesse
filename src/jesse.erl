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
        , del_schema/1
        , update_schema/2
        , update_schema/4
        , validate/2
        ]).

-export_type([ json_term/0
             ]).

-type json_term() :: term().

%%% API
%% @doc Adds a schema definition `Schema' to in-memory storage associated with
%% a key `Key'. It will overwrite an existing schema with the same key if
%% there is any.
-spec add_schema(Key :: any(), Schema :: json_term()) -> ok.
add_schema(Key, Schema) ->
  ValidationFun = fun jesse_schema_validator:is_json_object/1,
  MakeKeyFun    = fun(_) -> Key end,
  jesse_database:add_schema(Schema, ValidationFun, MakeKeyFun).

%% @doc Deletes a schema definition from in-memory storage associated with
%% the key `Key'.
-spec del_schema(Key :: any()) -> ok.
del_schema(Key) ->
  jesse_database:del_schema(Key).

%% @doc Updates schema definitions in in-memory storage.
%%
%% Equivalent to `update_schema(Path, ParseFun, ValidationFun, MakeKeyFun)'
%% where `ValidationFun' is `fun jesse_json:is_json_object/1' and
%% `MakeKeyFun' is `fun jesse_schema_validator:get_schema_id/1'.
-spec update_schema( Path     :: string()
                   , ParseFun :: fun((any()) -> json_term())
                   ) -> jesse_database:update_result().
update_schema(Path, ParseFun) ->
  update_schema( Path
               , ParseFun
               , fun jesse_schema_validator:is_json_object/1
               , fun jesse_schema_validator:get_schema_id/1
               ).

%% @doc Updates schema definitions in in-memory storage. The function loads all
%% the files from directory `Path', then each schema entry will be checked
%% for a validity by function `ValidationFun', and will be stored in in-memory
%% storage with a key returned by `MakeKeyFun' function.
%%
%% In addition to a schema definition, a timestamp of the schema file will be
%% stored, so, during the next update timestamps will be compared to avoid
%% unnecessary updates.
%%
%% Schema definitions are stored in the format which json parsing function
%% `ParseFun' returns.
-spec update_schema( Path          :: string()
                   , ParseFun      :: fun((any()) -> json_term())
                   , ValidationFun :: fun((any()) -> boolean())
                   , MakeKeyFun    :: fun((json_term()) -> any())
                   ) -> jesse_database:update_result().
update_schema(Path, ParseFun, ValidationFun, MakeKeyFun) ->
  jesse_database:update(Path, ParseFun, ValidationFun, MakeKeyFun).

%% @doc Validates json `Data' against a schema with the same key as `Schema'
%% in the internal storage. If the given json is valid, then it is returned
%% to the caller, otherwise an error with an appropriate error reason
%% is returned.
-spec validate(Schema :: any(), Data :: json_term()) -> {ok, json_term()}
                                                      | {error, term()}.
validate(Schema, Data) ->
  try
    JsonSchema = jesse_database:read_schema(Schema),
    jesse_schema_validator:validate(JsonSchema, Data)
  catch
    throw:Error ->
      {error, Error}
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
