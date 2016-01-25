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
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_state).

%% API
-export([ add_to_path/2
        , get_allowed_errors/1
        , get_current_path/1
        , get_current_schema/1
        , get_default_schema_ver/1
        , get_error_handler/1
        , get_error_list/1
        , new/2
        , remove_last_from_path/1
        , set_allowed_errors/2
        , set_current_schema/2
        , set_error_list/2
        , find_schema/2
        , resolve_reference/2
        ]).

-export_type([ state/0
             ]).

%% Includes
-include("jesse_schema_validator.hrl").

-define(schema_loader_fun, fun jesse_database:read/1).

%% Internal datastructures
-record( state
       , { root_schema        :: jesse:json_term()
         , current_schema     :: jesse:json_term()
         , current_path       :: [binary()] %% current path in reversed order
         , allowed_errors     :: non_neg_integer() | 'infinity'
         , error_list         :: list()
         , error_handler      :: fun((#state{}) -> list() | no_return())
         , default_schema_ver :: atom()
         , schema_loader_fun  :: fun((binary()) -> {ok, jesse:json_term()} | jesse:json_term() | ?not_found)
         , id                 :: binary()
         }
       ).

-opaque state() :: #state{}.

%%% API
%% @doc Adds `Property' to the `current_path' in `State'.
-spec add_to_path(State :: state(), Property :: binary()) -> state().
add_to_path(State, Property) ->
  CurrentPath = State#state.current_path,
  State#state{current_path = [Property | CurrentPath]}.

%% @doc Getter for `allowed_errors'.
-spec get_allowed_errors(State :: state()) -> non_neg_integer().
get_allowed_errors(#state{allowed_errors = AllowedErrors}) ->
  AllowedErrors.

%% @doc Getter for `current_path'.
-spec get_current_path(State :: state()) -> [binary()].
get_current_path(#state{current_path = CurrentPath}) ->
  CurrentPath.

%% @doc Getter for `current_schema'.
-spec get_current_schema(State :: state()) -> jesse:json_term().
get_current_schema(#state{current_schema = CurrentSchema}) ->
  CurrentSchema.

%% @doc Getter for `default_schema_ver'.
-spec get_default_schema_ver(State :: state()) -> binary().
get_default_schema_ver(#state{default_schema_ver = SchemaVer}) ->
  SchemaVer.

%% @doc Getter for `error_handler'.
-spec get_error_handler(State :: state()) ->
                           fun((#state{}) -> list() | no_return()).
get_error_handler(#state{error_handler = ErrorHandler}) ->
  ErrorHandler.

%% @doc Getter for `error_list'.
-spec get_error_list(State :: state()) -> list().
get_error_list(#state{error_list = ErrorList}) ->
  ErrorList.

%% @doc Returns newly created state.
-spec new( JsonSchema :: jesse:json_term()
         , Options    :: [{Key :: atom(), Data :: any()}]
         ) -> state().
new(JsonSchema, Options) ->
  DefaultHandler   = fun jesse_error:default_error_handler/3,
  ErrorHandler     = proplists:get_value( error_handler
                                        , Options
                                        , DefaultHandler
                                        ),
  AllowedErrors    = proplists:get_value( allowed_errors
                                        , Options
                                        , 0
                                        ),
  DefaultSchemaVer = proplists:get_value( default_schema_ver
                                        , Options
                                        , ?default_schema_ver
                                        ),
  LoaderFun = proplists:get_value( schema_loader_fun
                                 , Options
                                 , ?schema_loader_fun
                                 ),
  NewState = #state{ root_schema        = JsonSchema
                   , current_path       = []
                   , allowed_errors     = AllowedErrors
                   , error_list         = []
                   , error_handler      = ErrorHandler
                   , default_schema_ver = DefaultSchemaVer
                   , schema_loader_fun  = LoaderFun
                   },
  set_current_schema(NewState, JsonSchema).

%% @doc Removes the last element from `current_path' in `State'.
-spec remove_last_from_path(State :: state()) -> state().
remove_last_from_path(State = #state{current_path = [_Property | Path]}) ->
  State#state{current_path = Path}.

%% @doc Getter for `allowed_errors'.
-spec set_allowed_errors( State :: state()
                        , AllowedErrors :: non_neg_integer()
                        ) -> state().
set_allowed_errors(#state{} = State, AllowedErrors) ->
  State#state{allowed_errors = AllowedErrors}.

%% @doc Setter for `current_schema'.
-spec set_current_schema( State     :: state()
                        , NewSchema :: jesse:json_term()
                        ) -> state().
set_current_schema(#state{id = Id} = State, NewSchema) ->
  NewId = combine_id(Id, jesse_json_path:value(?ID, NewSchema, undefined)),
  State#state{current_schema = NewSchema, id = NewId}.

%% @doc Setter for `error_list'.
-spec set_error_list(State :: state(), ErrorList :: list()) -> state().
set_error_list(State, ErrorList) ->
  State#state{error_list = ErrorList}.

%% @doc Resolve a reference
-spec resolve_reference(State :: state(), Reference :: binary()) -> state().
resolve_reference(State, Reference) ->
  case combine_id(State#state.id, Reference) of
    %% Local references
    [$# | Pointer] ->
      Path = jesse_json_path:parse(Pointer),
      case local_schema(State#state.root_schema, Path) of
        ?not_found -> jesse_error:handle_schema_invalid(?schema_invalid, State);
        Schema     -> set_current_schema(State, Schema)
      end;
    %% Remote references
    RemoteURI ->
      %% Split the URI on the fragment if it exists
      [BaseURI | MaybePointer] = re:split(RemoteURI, <<$#>>, [{return, list}]),
      case jesse_state:find_schema(State, BaseURI) of
        ?not_found ->
          jesse_error:handle_schema_invalid(?schema_invalid, State);
        RemoteSchema ->
          %% Set the new root schema
          NewState = State#state{root_schema = RemoteSchema, id = BaseURI},
          %% Retrive the part we want
          Path = case MaybePointer of
                   []        -> [];
                   [Pointer] -> jesse_json_path:parse(Pointer)
                 end,
          case local_schema(RemoteSchema, Path) of
            ?not_found ->
              jesse_error:handle_schema_invalid(?schema_invalid, State);
            Schema ->
              set_current_schema(NewState, Schema)
          end
      end
  end.

%% @doc Retrive a specific part of a schema
%% @private
local_schema(?not_found, _Path) ->
  ?not_found;
local_schema(Schema, []) ->
  case jesse_lib:is_json_object(Schema) of
    true  -> Schema;
    false -> ?not_found
  end;
local_schema(Schema, [<<>> | Keys]) -> local_schema(Schema, Keys);
local_schema(Schema, [Key | Keys]) ->
  case jesse_lib:is_json_object(Schema) of
    true  ->
      SubSchema = jesse_json_path:value(Key, Schema, ?not_found),
      local_schema(SubSchema, Keys);
    false ->
      case jesse_lib:is_array(Schema) of
        true ->
          try binary_to_integer(Key) of
            Index ->
              SubSchema = lists:nth(Index + 1, Schema),
              local_schema(SubSchema, Keys)
          catch
            _:_ -> ?not_found
          end;
        false ->
          ?not_found
      end
  end.

%% @doc Resolve a new id
%% @private
-spec combine_id(undefined | http_uri:uri(), undefined | binary()) -> http_uri:uri().
combine_id(Id, undefined) ->
  Id;
combine_id(Id, Ref) ->
  RefStr = unicode:characters_to_list(Ref),
  case http_uri:parse(RefStr) of
    %% Absolute
    {ok, _} -> RefStr;
    %% Relative
    _Error  -> combine_relative_id(Id, RefStr)
  end.

combine_relative_id(undefined, Id) ->
  Id;
combine_relative_id(Id, [$# | Fragment]) ->
  [WithoutFragment | _] = re:split(Id, <<$#>>, [{return, list}]),
  WithoutFragment ++ [$# | Fragment];
combine_relative_id(Id, NewFile) ->
  BaseURI = filename:dirname(Id),
  FileName = unicode:characters_to_list(NewFile),
  BaseURI ++ [$/ | FileName].

%% @doc Find a schema based on URI
-spec find_schema(State :: state(), SchemaURI :: binary()) ->
    jesse:json_term() | ?not_found.
find_schema(#state{schema_loader_fun=LoaderFun}, SchemaURI) ->
  try LoaderFun(SchemaURI) of
      {ok, Schema} ->
        Schema;
      Schema ->
        case jesse_lib:is_json_object(Schema) of
          true  -> Schema;
          false -> ?not_found
        end
  catch
    _:_ -> ?not_found
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
