%%%=============================================================================
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_schema_validator).

%% API
-export([ validate/2
        , get_schema_id/1
        , is_json_object/1
        ]).

%% Constant definitions for Json schema keywords
-define(TYPE,                 <<"type">>).
-define(PROPERTIES,           <<"properties">>).
-define(PATTERNPROPERTIES,    <<"patternProperties">>).    % NOT IMPLEMENTED YET
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).         % NOT IMPLEMENTED YET
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).              % NOT IMPLEMENTED YET
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(DEFAULT,              <<"default">>).              % NOT IMPLEMENTED YET
-define(TITLE,                <<"title">>).                % NOT IMPLEMENTED YET
-define(DESCRIPTION,          <<"description">>).          % NOT IMPLEMENTED YET
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).             % NOT IMPLEMENTED YET
-define(EXTENDS,              <<"extends">>).
-define(ID,                   <<"id">>).
-define(_REF,                 <<"$ref">>).                 % NOT IMPLEMENTED YET
-define(_SCHEMA,              <<"$schema">>).              % NOT IMPLEMENTED YET
-define(LINKS,                <<"links">>).                % NOT IMPLEMENTED YET
-define(HREF,                 <<"href">>).                 % NOT IMPLEMENTED YET
-define(REL,                  <<"rel">>).                  % NOT IMPLEMENTED YET
-define(TARGETSCHEMA,         <<"targetSchema">>).         % NOT IMPLEMENTED YET
-define(METHOD,               <<"method">>).               % NOT IMPLEMENTED YET
-define(ENCTYPE,              <<"enctype">>).              % NOT IMPLEMENTED YET
-define(SCHEMA,               <<"schema">>).               % NOT IMPLEMENTED YET
-define(FRAGMENTRESOLUTION,   <<"fragmentResolution">>).   % NOT IMPLEMENTED YET
-define(READONLY,             <<"readonly">>).             % NOT IMPLEMENTED YET
-define(CONTENTENCODING,      <<"contentEncoding">>).      % NOT IMPLEMENTED YET
-define(PATHSTART,            <<"pathStart">>).            % NOT IMPLEMENTED YET
-define(MEDIATYPE,            <<"mediaType">>).            % NOT IMPLEMENTED YET

%% Constant definitions for Json types
-define(ANY,                  <<"any">>).
-define(ARRAY,                <<"array">>).
-define(BOOLEAN,              <<"boolean">>).
-define(INTEGER,              <<"integer">>).
-define(NULL,                 <<"null">>).
-define(NUMBER,               <<"number">>).
-define(OBJECT,               <<"object">>).
-define(STRING,               <<"string">>).

%%% API
%% @doc Validates json `Data' against `Schema'. If the given json is valid,
%% then it is returned to the caller as is, otherwise an exception
%% will be thrown.
-spec validate( JsonSchema :: jesse:json_term()
              , Data       :: jesse:json_term()
              ) -> {ok, jesse:json_term()}
                 | no_return().
validate(JsonSchema, Data) ->
  {check_property({none, Data}, JsonSchema), Data}.

%% @doc Returns value of "id" field from json object `Schema', assuming that
%% the given json object has such a field, otherwise an exception
%% will be thrown.
-spec get_schema_id(Schema :: jesse:json_term()) -> string().
get_schema_id(Schema) ->
  case jesse_json_path:path(?ID, Schema) of
    [] ->
      throw({schema_invalid, Schema, missing_id_field});
    Id ->
      erlang:binary_to_list(Id)
  end.

%% @doc A naive check if the given data is a json object.
%% Supports two main formats of json representation:
%% 1) mochijson format (`{struct, proplist()}')
%% 2) EEP18 format (`{proplist()}')
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(any()) -> boolean().
is_json_object({struct, Value}) when is_list(Value) -> true;
is_json_object({Value}) when is_list(Value)         -> true;
is_json_object(_)                                   -> false.

%%% Internal functions
%% @private
validate_any(Property, Schema) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  ok.

%% @private
validate_array({_Key, Value} = Property, Schema) when is_list(Value) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  check_enum(Property, get_path(?ENUM, Schema)),
  check_min_items(Property, get_path(?MINITEMS, Schema)),
  check_max_items(Property, get_path(?MAXITEMS, Schema)),
  check_items( Property
             , get_path(?ITEMS, Schema)
             , Schema
             ),
  check_unique_items( Property
                    , get_path(?UNIQUEITEMS, Schema)
                    ),
  ok;
validate_array(Property, Schema) ->
  throw({data_invalid, Property, not_array, Schema}).

%% @private
validate_boolean({_Key, Value} = Property, Schema) when is_boolean(Value) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  check_enum(Property, get_path(?ENUM, Schema)),
  ok;
validate_boolean(Property, Schema) ->
  throw({data_invalid, Property, not_boolean, Schema}).

%% @private
validate_integer({_Key, Value} = Property, Schema) when is_integer(Value) ->
  validate_number(Property, Schema);
validate_integer(Property, Schema) ->
  throw({data_invalid, Property, not_integer, Schema}).

%% @private
validate_null({_Key, null} = Property, Schema) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  ok;
validate_null(Property, Schema) ->
  throw({data_invalid, Property, not_null, Schema}).

%% @private
validate_number({_Key, Value} = Property, Schema) when is_number(Value) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  check_enum(Property, get_path(?ENUM, Schema)),
  check_min( Property
           , get_path(?MINIMUM, Schema)
           , get_path(?EXCLUSIVEMINIMUM, Schema)
           ),
  check_max( Property
           , get_path(?MAXIMUM, Schema)
           , get_path(?EXCLUSIVEMAXIMUM, Schema)
           ),
  check_divisible_by( Property
                    , get_path(?DIVISIBLEBY, Schema)
                    , Schema
                    ),
  ok;
validate_number(Property, Schema) ->
  throw({data_invalid, Property, not_number, Schema}).

%% @private
validate_object({_Key, Value} = Property, Schema) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  case is_json_object(Value) of
    true ->
      check_enum(Property, get_path(?ENUM, Schema)),
      Properties       = unwrap(Value),
      PropertiesSchema = get_path(?PROPERTIES, Schema),
      check_required(Properties, PropertiesSchema),
      check_properties(Properties, Schema);
    false ->
      throw({data_invalid, Property, not_object, Schema})
  end.

%% @private
validate_string({_Key, Value} = Property, Schema) when is_binary(Value) ->
  check_extends(Property, get_path(?EXTENDS, Schema)),
  check_enum(Property, get_path(?ENUM, Schema)),
  check_min_length(Property, get_path(?MINLENGTH, Schema)),
  check_max_length(Property, get_path(?MAXLENGTH, Schema)),
  ok;
validate_string(Property, Schema) ->
  throw({data_invalid, Property, not_string, Schema}).

%%=============================================================================
%% @private
check_extends(_Property, []) ->
  ok;
check_extends(Property, Schemas) when is_list(Schemas) ->
  ForeachFun = fun(Schema) ->
                   check_extends(Property, Schema)
               end,
  lists:foreach(ForeachFun, Schemas);
check_extends(Property, Schema) ->
  check_property(Property, jesse_database:read_schema(Schema)).

%% @private
check_properties([], _Schema) ->
  ok;
check_properties([{Key, _Value} = Property | Rest], Schema) ->
  PropertiesSchema = get_path(?PROPERTIES, Schema),
  case check_property(Property, get_path(Key, PropertiesSchema)) of
    ok ->
      check_properties(Rest, Schema);
    extra_property ->
      check_additional_property(Property, Schema),
      check_properties(Rest, Schema)
  end.

%% @private
check_property({?LINKS, _} = _Property, _) ->
  %% TODO: add handling for "links"
  ok;
check_property(_Property, []) ->
  extra_property;
check_property(Property, Schema) ->
  case is_json_object(Schema) of
    true ->
      SchemaType = get_path(?TYPE, Schema),
      check_type(SchemaType, Property, Schema);
    false ->
      throw({schema_invalid, Schema, not_object})
  end.

%% @private
check_type([],        Property, Schema) -> validate_any(Property, Schema);
check_type(?ANY,      Property, Schema) -> validate_any(Property, Schema);
check_type(?ARRAY,    Property, Schema) -> validate_array(Property, Schema);
check_type(?BOOLEAN,  Property, Schema) -> validate_boolean(Property, Schema);
check_type(?INTEGER,  Property, Schema) -> validate_integer(Property, Schema);
check_type(?NULL,     Property, Schema) -> validate_null(Property, Schema);
check_type(?NUMBER,   Property, Schema) -> validate_number(Property, Schema);
check_type(?OBJECT,   Property, Schema) -> validate_object(Property, Schema);
check_type(?STRING,   Property, Schema) -> validate_string(Property, Schema);
check_type(UnionType, Property, Schema)
  when is_list(UnionType) ->
  IsValid = lists:any( fun(Type) ->
                           try
                             ok =:= check_type(Type, Property, Schema)
                           catch
                             throw:{data_invalid, _, _, _} -> false;
                             throw:{schema_invalid, _, _} -> false
                           end
                       end
                     , UnionType
                     ),
  case IsValid of
    true -> ok;
    false -> throw({data_invalid, Property, not_correct_type, Schema})
  end;
check_type(_Type, Property, Schema) -> validate_any(Property, Schema).

%% @private
check_enum(_Property, []) ->
  ok;
check_enum({_Key, Value} = Property, Enum) ->
  IsValid = lists:any( fun(ExpectedValue) ->
                           Value =:= ExpectedValue
                       end
                     , Enum
                     ),
  case IsValid of
    true -> ok;
    false -> throw({data_invalid, Property, not_in_enum, Enum})
  end.

%% @private
check_min(_Property, [], _Excl) ->
  ok;
check_min({_Key, Value}, Min, true) when Value > Min ->
  ok;
check_min({_Key, Value}, Min, _Excl) when Value >= Min ->
  ok;
check_min(Property, Min, Excl) ->
  throw({ data_invalid
        , Property
        , not_in_range
        , {{minimum, Min}, {exclusive, Excl}}
        }).

%% @private
check_max(_Property, [], _Excl) ->
  ok;
check_max({_Key, Value}, Max, true) when Value < Max ->
  ok;
check_max({_Key, Value}, Max, _Excl) when Value =< Max ->
  ok;
check_max(Property, Max, Excl) ->
  throw({ data_invalid
        , Property
        , not_in_range
        , {{maximum, Max}, {exclusive, Excl}}
        }).

%% @private
check_divisible_by(_Property, [], _ParentSchema) ->
  ok;
check_divisible_by(_Property, 0, ParentSchema) ->
  throw({ schema_invalid
        , ParentSchema
        , {divide_by, 0}
        });
check_divisible_by({_Key, Value} = Property, Divider, _ParentSchema) ->
  case Value rem Divider of
    0 ->
      ok;
    _ ->
      throw({ data_invalid
            , Property
            , not_divisible_by
            , Divider
            })
  end.

%% @private
check_min_length(_Property, []) ->
  ok;
check_min_length({_Key, Value} = Property, Min) ->
  case length(unicode:characters_to_list(Value)) >= Min of
    true ->
      ok;
    false ->
      throw({ data_invalid
            , Property
            , not_correct_length
            , {min_length, Min}
            })
  end.

%% @private
check_max_length(_Property, []) ->
  ok;
check_max_length({_Key, Value} = Property, Max) ->
  case length(unicode:characters_to_list(Value)) =< Max of
    true ->
      ok;
    false ->
      throw({ data_invalid
            , Property
            , not_correct_length
            , {max_length, Max}
            })
  end.

%% @private
check_min_items(_Property, []) ->
  ok;
check_min_items({_Key, Array}, Min) when length(Array) >= Min ->
  ok;
check_min_items(Property, Min) ->
  throw({ data_invalid
        , Property
        , not_correct_size
        , {min_items, Min}
        }).

%% @private
check_max_items(_Property, []) ->
  ok;
check_max_items({_Key, Array}, Max) when length(Array) =< Max ->
  ok;
check_max_items(Property, Max) ->
  throw({ data_invalid
        , Property
        , not_correct_size
        , {max_items, Max}
        }).

%% @private
check_items(_Property, [], _ParentSchema) ->
  ok;
check_items(Property, Schema, ParentSchema) when is_list(Schema) ->
  check_array_schema(Property, Schema, ParentSchema);
check_items({Key, Values} = _Property, Schema, _ParentSchema) ->
  lists:foreach( fun(Value) ->
                     check_property({Key, Value}, Schema)
                 end
               , Values
               ).

%% @private
check_array_schema({_Key, []}, _Schema, _ParentSchema) ->
  ok;
check_array_schema({Key, [Item | Rest]}, [], ParentSchema) ->
  case get_path(?ADDITIONALITEMS, ParentSchema) of
    false ->
      throw({ data_invalid
            , {Key, Item}
            , no_extra_items_allowed
            , ParentSchema
            });
    true ->
      ok;
    [] ->
      ok;
    AdditionalSchema ->
      check_property({Key, Item}, AdditionalSchema),
      check_array_schema({Key, Rest}, [], ParentSchema)
  end;
check_array_schema({Key, [Item | RestItems]}, [Schema | Rest], ParentSchema) ->
  check_property({Key, Item}, Schema),
  check_array_schema({Key, RestItems}, Rest, ParentSchema).

%% @private
check_unique_items(_Property, []) ->
  ok;
check_unique_items(_Property, false) ->
  ok;
check_unique_items({_Key, []}, _UniqueItems) ->
  ok;
check_unique_items({_Key, [_Item]}, _UniqueItems) ->
  ok;
check_unique_items({_Key, [_Item1, _Item2]}, _UniqueItems) ->
  ok;
check_unique_items({Key, [Item, Item | _Rest]}, UniqueItems) ->
  throw({ data_invalid
        , {Key, Item}
        , not_unique_item
        , {unique_items, UniqueItems}
        });
check_unique_items({Key, [Item1, Item2 | Rest]}, UniqueItems) ->
  check_unique_items({Key, [Item1, Rest]}, UniqueItems),
  check_unique_items({Key, [Item2, Rest]}, UniqueItems).

%% @private
check_required(Properties, PropertiesSchema) ->
  Required = get_required(to_proplist(PropertiesSchema)),
  lists:foreach( fun(Name) ->
                     case get_path(Name, Properties) of
                       [] ->
                         throw({ data_invalid
                               , Properties
                               , missing_required_property
                               , Name
                               });
                       _RequiredProperty ->
                         true
                     end
                 end
               , Required
               ),
  ok.

%% @private
get_required(PropertiesSchema) ->
  lists:filter( fun(Property) ->
                    true =:= get_path(?REQUIRED, Property)
                end
              , PropertiesSchema
              ).

%% @private
check_additional_property(Property, Schema) ->
  case get_path(?ADDITIONALPROPERTIES, Schema) of
    false ->
      throw({ data_invalid
            , Property
            , no_extra_properties_allowed
            , Schema
            });
    true ->
      ok;
    [] ->
      ok;
    AdditionalSchema ->
      check_property(Property, AdditionalSchema)
  end.

%%=============================================================================
%% @private
get_path(Key, Schema) ->
  jesse_json_path:path(Key, Schema).


%% @private
to_proplist(Value) ->
  jesse_json_path:to_proplist(Value).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
