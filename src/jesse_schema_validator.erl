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
-define(PATTERNPROPERTIES,    <<"patternProperties">>).
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(DEFAULT,              <<"default">>).              % NOT IMPLEMENTED YET
-define(TITLE,                <<"title">>).                % NOT IMPLEMENTED YET
-define(DESCRIPTION,          <<"description">>).          % NOT IMPLEMENTED YET
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).
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
validate(JsonSchema, Value) ->
  {check_value(Value, unwrap(JsonSchema), JsonSchema), Value}.

%% @doc Returns value of "id" field from json object `Schema', assuming that
%% the given json object has such a field, otherwise an exception
%% will be thrown.
-spec get_schema_id(Schema :: jesse:json_term()) -> string().
get_schema_id(Schema) ->
  case get_path(?ID, Schema) of
    [] -> throw({schema_invalid, Schema, missing_id_field});
    Id -> erlang:binary_to_list(Id)
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
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
%% @private
check_value(Value, [{?TYPE, Type} | Attrs], JsonSchema) ->
  check_type(Value, Type, JsonSchema),
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?PROPERTIES, Properties} | Attrs], JsonSchema) ->
  case is_json_object(Value) of
    true  -> check_properties(Value, unwrap(Properties));
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value( Value
           , [{?PATTERNPROPERTIES, PatternProperties} | Attrs]
           , JsonSchema
           ) ->
  case is_json_object(Value) of
    true  -> check_pattern_properties(Value, PatternProperties);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value( Value
           , [{?ADDITIONALPROPERTIES, AdditionalProperties} | Attrs]
           , JsonSchema
           ) ->
  case is_json_object(Value) of
    true  -> check_additional_properties( Value
                                        , AdditionalProperties
                                        , JsonSchema
                                        );
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?ITEMS, Items} | Attrs], JsonSchema) ->
  case is_list(Value) of
    true  -> check_items(Value, Items, JsonSchema);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
           , JsonSchema
           ) ->
  check_value(Value, Attrs, JsonSchema);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(Value, [{?REQUIRED, _Required} | Attrs], JsonSchema) ->
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?DEPENDENCIES, Dependencies} | Attrs], JsonSchema) ->
  case is_json_object(Value) of
    true  -> check_dependencies(Value, Dependencies);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MINIMUM, Minimum} | Attrs], JsonSchema) ->
  case is_number(Value) of
    true  ->
      ExclusiveMinimum = get_path(?EXCLUSIVEMINIMUM, JsonSchema),
      check_minimum(Value, Minimum, ExclusiveMinimum);
    false ->
      ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MAXIMUM, Maximum} | Attrs], JsonSchema) ->
  case is_number(Value) of
    true  ->
      ExclusiveMaximum = get_path(?EXCLUSIVEMAXIMUM, JsonSchema),
      check_maximum(Value, Maximum, ExclusiveMaximum);
    false ->
      ok
  end,
  check_value(Value, Attrs, JsonSchema);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMINIMUM, _ExclusiveMinimum} | Attrs]
           , JsonSchema
           ) ->
  check_value(Value, Attrs, JsonSchema);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMAXIMUM, _ExclusiveMaximum} | Attrs]
           , JsonSchema
           ) ->
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MINITEMS, MinItems} | Attrs], JsonSchema) ->
  case is_list(Value) of
    true  -> check_min_items(Value, MinItems);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], JsonSchema) ->
  case is_list(Value) of
    true  -> check_max_items(Value, MaxItems);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], JsonSchema) ->
  case is_list(Value) of
    true  -> check_unique_items(Value, Uniqueitems);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?PATTERN, Pattern} | Attrs], JsonSchema) ->
  case is_binary(Value) of
    true  -> check_pattern(Value, Pattern);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MINLENGTH, MinLength} | Attrs], JsonSchema) ->
  case is_binary(Value) of
    true  -> check_min_length(Value, MinLength);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?MAXLENGTH, MaxLength} | Attrs], JsonSchema) ->
  case is_binary(Value) of
    true  -> check_max_length(Value, MaxLength);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?ENUM, Enum} | Attrs], JsonSchema) ->
  check_enum(Value, Enum),
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?FORMAT, Format} | Attrs], JsonSchema) ->
  check_format(Value, Format),
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?DIVISIBLEBY, DivisibleBy} | Attrs], JsonSchema) ->
  case is_number(Value) of
    true  -> check_divisible_by(Value, DivisibleBy, JsonSchema);
    false -> ok
  end,
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?DISALLOW, Disallow} | Attrs], JsonSchema) ->
  check_disallow(Value, Disallow, JsonSchema),
  check_value(Value, Attrs, JsonSchema);
check_value(Value, [{?EXTENDS, Extends} | Attrs], JsonSchema) ->
  check_extends(Value, Extends),
  check_value(Value, Attrs, JsonSchema);
check_value(_Value, [], _JsonSchema) ->
  ok;
check_value(Value, [_Attr | Attrs], JsonSchema) ->
  check_value(Value, Attrs, JsonSchema).

%% @doc 5.1.  type
%%
%% This attribute defines what the primitive type or the schema of the
%% instance MUST be in order to validate.  This attribute can take one
%% of two forms:
%%
%% Simple Types  A string indicating a primitive or simple type. The
%%    following are acceptable string values:
%%
%%    string  Value MUST be a string.
%%
%%    number  Value MUST be a number, floating point numbers are
%%       allowed.
%%
%%    integer  Value MUST be an integer, no floating point numbers are
%%       allowed.  This is a subset of the number type.
%%
%%    boolean  Value MUST be a boolean.
%%
%%    object  Value MUST be an object.
%%
%%    array  Value MUST be an array.
%%
%%    null  Value MUST be null.  Note this is mainly for purpose of
%%       being able use union types to define nullability.  If this type
%%       is not included in a union, null values are not allowed (the
%%       primitives listed above do not allow nulls on their own).
%%
%%    any  Value MAY be of any type including null.
%%
%%    If the property is not defined or is not in this list,
%%    then any type of value is acceptable.  Other type values MAY be used for
%%    custom purposes, but minimal validators of the specification
%%    implementation can allow any instance value on unknown type
%%    values.
%%
%%  Union Types  An array of two or more simple type definitions.  Each
%%     item in the array MUST be a simple type definition or a schema.
%%     The instance value is valid if it is of the same type as one of
%%     the simple type definitions, or valid by one of the schemas, in
%%     the array.
%%
%%  For example, a schema that defines if an instance can be a string or
%%  a number would be:
%%
%%  {"type":["string","number"]}
%% @private
check_type(Value, ?STRING, JsonSchema) ->
  case is_binary(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_string, JsonSchema})
  end;
check_type(Value, ?NUMBER, JsonSchema) ->
  case is_number(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_number, JsonSchema})
  end;
check_type(Value, ?INTEGER, JsonSchema) ->
  case is_integer(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_integer, JsonSchema})
  end;
check_type(Value, ?BOOLEAN, JsonSchema) ->
  case is_boolean(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_boolean, JsonSchema})
  end;
check_type(Value, ?OBJECT, JsonSchema) ->
  case is_json_object(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_object, JsonSchema})
  end;
check_type(Value, ?ARRAY, JsonSchema) ->
  case is_list(Value) of
    true  -> ok;
    false -> throw({data_invalid, Value, not_array, JsonSchema})
  end;
check_type(Value, ?NULL, JsonSchema) ->
  case Value of
    null -> ok;
    _    -> throw({data_invalid, Value, not_null, JsonSchema})
  end;
check_type(_Value, ?ANY, _JsonSchema) ->
  ok;
check_type(Value, UnionType, JsonSchema) when is_list(UnionType) ->
  IsValid = lists:any( fun(Type) ->
                           try
                             case is_json_object(Type) of
                               true ->
                                 %% case when there's a schema in the array,
                                 %% then we need to validate against
                                 %% that schema
                                 ok =:= check_value( Value
                                                   , unwrap(Type)
                                                   , Type
                                                   );
                               false ->
                                 ok =:= check_type(Value, Type, JsonSchema)
                             end
                           catch
                             throw:{data_invalid, _, _, _} -> false;
                             throw:{schema_invalid, _, _}  -> false
                           end
                       end
                     , UnionType
                     ),
  case IsValid of
    true  -> ok;
    false -> throw({data_invalid, Value, not_correct_type, JsonSchema})
  end;
check_type(_Value, _Type, _JsonSchema) ->
  ok.

%% @doc 5.2.  properties
%%
%% This attribute is an object with property definitions that define the
%% valid values of instance object property values.  When the instance
%% value is an object, the property values of the instance object MUST
%% conform to the property definitions in this object.  In this object,
%% each property definition's value MUST be a schema, and the property's
%% name MUST be the name of the instance property that it defines.  The
%% instance property value MUST be valid according to the schema from
%% the property definition.  Properties are considered unordered, the
%% order of the instance properties MAY be in any order.
%% @private
check_properties(Value, Properties) ->
  lists:foreach( fun({PropertyName, PropertySchema}) ->
                     case get_path(PropertyName, Value) of
                       [] ->
%% @doc 5.7.  required
%%
%% This attribute indicates if the instance must have a value, and not
%% be undefined.  This is false by default, making the instance
%% optional.
%% @end
                         case get_path(?REQUIRED, PropertySchema) of
                           true  -> throw({ data_invalid
                                          , Value
                                          , missing_required_property
                                          , PropertyName
                                          });
                           _     -> ok
                         end;
                       Property -> check_value( Property
                                               , unwrap(PropertySchema)
                                               , PropertySchema
                                               )
                     end
                 end
               , Properties
               ).

%% @doc 5.3.  patternProperties
%%
%% This attribute is an object that defines the schema for a set of
%% property names of an object instance.  The name of each property of
%% this attribute's object is a regular expression pattern in the ECMA
%% 262/Perl 5 format, while the value is a schema.  If the pattern
%% matches the name of a property on the instance object, the value of
%% the instance's property MUST be valid against the pattern name's
%% schema value.'
%% @private
check_pattern_properties(Value, PatternProperties) ->
  [ check_match({PropertyName, PropertyValue}, {Pattern, Schema})
   || {Pattern, Schema} <- unwrap(PatternProperties),
      {PropertyName, PropertyValue} <- unwrap(Value)],
  ok.

%% @private
check_match({PropertyName, PropertyValue}, {Pattern, Schema}) ->
  case re:run(PropertyName, Pattern, [{capture, none}]) of
    match   -> check_value(PropertyValue, unwrap(Schema), Schema);
    nomatch -> ok
  end.

%% @doc 5.4.  additionalProperties
%%
%% This attribute defines a schema for all properties that are not
%% explicitly defined in an object type definition.  If specified,
%% the value MUST be a schema or a boolean.  If false is provided,
%% no additional properties are allowed beyond the properties defined in
%% the schema.  The default value is an empty schema which allows any
%% value for additional properties.
%% @private
check_additional_properties(Value, false, JsonSchema) ->
  Properties        = get_path(?PROPERTIES, JsonSchema),
  PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []      -> ok;
    _Extras -> throw({ data_invalid
                     , {Value, _Extras}
                     , no_extra_properties_allowed
                     , JsonSchema
                     })
  end;
check_additional_properties(_Value, true, _JsonSchema) ->
  ok;
check_additional_properties(Value, AdditionalProperties, JsonSchema) ->
  Properties        = get_path(?PROPERTIES, JsonSchema),
  PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> ok;
    Extras -> lists:foreach( fun(Extra) ->
                                 check_value( Extra
                                            , unwrap(AdditionalProperties)
                                            , AdditionalProperties
                                            )
                             end
                           , Extras
                           )
  end.

%% @private
get_additional_properties(Value, Properties, PatternProperties) ->
  ValuePropertiesNames  = [Name || {Name, _} <- unwrap(Value)],
  SchemaPropertiesNames = [Name || {Name, _} <- unwrap(Properties)],
  Patterns    = [Pattern || {Pattern, _} <- unwrap(PatternProperties)],
  ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
  ExtraNames  = lists:foldl( fun(Pattern, ExtraAcc) ->
                                 filter_extra_names(Pattern, ExtraAcc)
                             end
                           , ExtraNames0
                           , Patterns
                           ),
  lists:map(fun(Name) -> get_path(Name, Value) end, ExtraNames).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case re:run(ExtraName, Pattern, [{capture, none}]) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc 5.5.  items
%%
%% This attribute defines the allowed items in an instance array,
%% and MUST be a schema or an array of schemas.  The default value is an
%% empty schema which allows any value for items in the instance array.
%%
%% When this attribute value is a schema and the instance value is an
%% array, then all the items in the array MUST be valid according to the
%% schema.
%%
%% When this attribute value is an array of schemas and the instance
%% value is an array, each position in the instance array MUST conform
%% to the schema in the corresponding position for this array.  This
%% called tuple typing.  When tuple typing is used, additional items are
%% allowed, disallowed, or constrained by the "additionalItems"
%% (Section 5.6) attribute using the same rules as 
%% "additionalProperties" (Section 5.4) for objects.
%% @private
check_items(Value, Items, JsonSchema) when is_list(Items) ->
  Tuples = case length(Value) - length(Items) of
             0 ->
               lists:zip(Value, Items);
             NExtra when NExtra > 0 ->
%% @doc 5.6.  additionalItems
%%
%% This provides a definition for additional items in an array instance
%% when tuple definitions of the items is provided.  This can be false
%% to indicate additional items in the array are not allowed, or it can
%% be a schema that defines the schema of the additional items.
%% @end
               case get_path(?ADDITIONALITEMS, JsonSchema) of
                 [] ->
                   [];
                 true ->
                   [];
                 false ->
                   throw({ data_invalid
                         , Value
                         , no_extra_items_allowed
                         , JsonSchema
                         });
                 AdditionalItems ->
                   ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
                   lists:zip(Value, lists:append(Items, ExtraSchemas))
               end;
             NExtra when NExtra < 0 ->
               throw({ data_invalid
                     , Value
                     , not_enought_items
                     , JsonSchema
                     })
           end,
  lists:foreach( fun({Item, Schema}) ->
                     check_value(Item, unwrap(Schema), Schema)
                 end
               , Tuples
               );
check_items(Value, Items, _JsonSchema) ->
  lists:foreach( fun(Item) ->
                     check_value(Item, unwrap(Items), Items)
                 end
               , Value
               ).

%% 5.8.  dependencies
%%
%% This attribute is an object that defines the requirements of a
%% property on an instance object.  If an object instance has a property
%% with the same name as a property in this attribute's object, then the
%% instance must be valid against the attribute's property value
%% (hereafter referred to as the "dependency value").
%%
%% The dependency value can take one of two forms:
%%
%% Simple Dependency  If the dependency value is a string,
%%    then the instance object MUST have a property with the same name as the
%%    dependency value.  If the dependency value is an array of strings,
%%    then the instance object MUST have a property with the same name
%%    as each string in the dependency value's array.
%%
%% Schema Dependency  If the dependency value is a schema, then the
%%    instance object MUST be valid against the schema.
%% @private
check_dependencies(Value, Dependencies) ->
  lists:foreach( fun({DependencyName, DependencyValue}) ->
                     case get_path(DependencyName, Value) of
                       [] -> ok;
                       _  -> check_dependency_value(Value, DependencyValue)
                     end
                 end
               , unwrap(Dependencies)
               ).

%% @private
check_dependency_value(Value, Dependency) when is_list(Dependency) ->
  lists:foreach( fun(PropertyName) ->
                     check_dependency_value(Value, PropertyName)
                 end
               , Dependency
               );
check_dependency_value(Value, Dependency) when is_binary(Dependency) ->
  case get_path(Dependency, Value) of
    [] -> throw({ data_invalid
                , Value
                , missing_dependency
                , Dependency
                });
    _  -> ok
  end;
check_dependency_value(Value, Dependency) ->
  case is_json_object(Dependency) of
    true  -> check_value(Value, unwrap(Dependency), Dependency);
    false -> throw({ schema_invalid
                   , Dependency
                   , wrong_type_dependency
                   })
  end.

%% @doc 5.9.  minimum
%%
%% This attribute defines the minimum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_minimum(Value, Minimum, ExclusiveMinimum) ->
%% @doc 5.11.  exclusiveMinimum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "minimum" attribute.  This is false by default, meaning the instance
%% value can be greater then or equal to the minimum value.
%% @end
  Result = case ExclusiveMinimum of
             true -> Value > Minimum;
             _    -> Value >= Minimum
           end,
  case Result of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_in_range
                   , {{minimum, Minimum}, {exclusive, ExclusiveMinimum}}
                   })
  end.

%%% @doc 5.10.  maximum
%%
%% This attribute defines the maximum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_maximum(Value, Maximum, ExclusiveMaximum) ->
%% @doc 5.12.  exclusiveMaximum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "maximum" attribute.  This is false by default, meaning the instance
%% value can be less then or equal to the maximum value.
%% @end
  Result = case ExclusiveMaximum of
             true -> Value < Maximum;
             _    -> Value =< Maximum
           end,
  case Result of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_in_range
                   , {{maximum, Maximum}, {exclusive, ExclusiveMaximum}}
                   })
  end.

%% @doc 5.13.  minItems
%%
%% This attribute defines the minimum number of values in an array when
%% the array is the instance value.
%% @private
check_min_items(Value, MinItems) when length(Value) >= MinItems ->
  ok;
check_min_items(Value, MinItems) ->
  throw({ data_invalid
        , Value
        , not_correct_size
        , {min_items, MinItems}
        }).

%% @doc 5.14.  maxItems
%%
%% This attribute defines the maximum number of values in an array when
%% the array is the instance value.
%% @private
check_max_items(Value, MaxItems) when length(Value) =< MaxItems ->
  ok;
check_max_items(Value, MaxItems) ->
  throw({ data_invalid
        , Value
        , not_correct_size
        , {max_items, MaxItems}
        }).

%% @doc 5.15.  uniqueItems
%%
%% This attribute indicates that all items in an array instance MUST be
%% unique (contains no two identical values).
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%%
%%    are null; or
%%
%%    are booleans/numbers/strings and have the same value; or
%%
%%    are arrays, contains the same number of items, and each item in
%%    the array is equal to the corresponding item in the other array;
%%    or
%%
%%    are objects, contains the same property names, and each property
%%    in the object is equal to the corresponding property in the other
%%    object.
%% @private
check_unique_items(Value, true = Uniqueitems) ->
  lists:foldl( fun(_Item, []) ->
                   ok;
                  (Item, RestItems) ->
                   lists:foreach( fun(ItemFromRest) ->
                                      case is_equal(Item, ItemFromRest) of
                                        true  -> throw({ data_invalid
                                                       , Value
                                                       , {Item, not_unique}
                                                       , { uniqueItems
                                                         , Uniqueitems
                                                         }
                                                       });
                                        false -> ok
                                      end
                                  end
                                , RestItems
                                ),
                   tl(RestItems)
               end
             , tl(Value)
             , Value
             ),
  ok.

%% @doc 5.16.  pattern
%% When the instance value is a string, this provides a regular
%% expression that a string instance MUST match in order to be valid.
%% Regular expressions SHOULD follow the regular expression
%% specification from ECMA 262/Perl 5
%% @private
check_pattern(Value, Pattern) ->
  case re:run(Value, Pattern, [{capture, none}]) of
    match   -> ok;
    nomatch -> throw({ data_invalid
                     , Value
                     , no_match
                     , Pattern
                     })
  end.

%% @doc 5.17.  minLength
%%
%% When the instance value is a string, this defines the minimum length
%% of the string.
%% @private
check_min_length(Value, MinLength) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_correct_length
                   , {min_length, MinLength}
                   })
  end.

%% @doc 5.18.  maxLength
%%
%% When the instance value is a string, this defines the maximum length
%% of the string.
%% @private
check_max_length(Value, MaxLength) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_correct_length
                   , {max_length, MaxLength}
                   })
  end.

%% @doc 5.19.  enum
%%
%% This provides an enumeration of all possible values that are valid
%% for the instance property.  This MUST be an array, and each item in
%% the array represents a possible value for the instance value.  If
%% this attribute is defined, the instance value MUST be one of the
%% values in the array in order for the schema to be valid.  Comparison
%% of enum values uses the same algorithm as defined in "uniqueItems"
%% (Section 5.15).
%% @private
check_enum(Value, Enum) ->
  IsValid = lists:any( fun(ExpectedValue) ->
                           is_equal(Value, ExpectedValue)
                       end
                     , Enum
                     ),
  case IsValid of
    true  -> ok;
    false -> throw({data_invalid, Value, not_in_enum, Enum})
  end.

%% TODO:
check_format(_Value, _Format) -> ok.

%% @doc 5.24.  divisibleBy
%%
%% This attribute defines what value the number instance must be
%% divisible by with no remainder (the result of the division must be an
%% integer.)  The value of this attribute SHOULD NOT be 0.
%% @private
check_divisible_by(_Value, 0, JsonSchema) ->
  throw({ schema_invalid
        , JsonSchema
        , {divide_by, 0}
        });
check_divisible_by(Value, DivisibleBy, _JsonSchema) ->
  Result = (Value / DivisibleBy - trunc(Value / DivisibleBy)) * DivisibleBy,
  case Result of
    0.0 -> ok;
    _   -> throw({ data_invalid
                 , Value
                 , not_divisible_by
                 , DivisibleBy
                 })
  end.

%% @doc 5.25.  disallow
%%
%% This attribute takes the same values as the "type" attribute, however
%% if the instance matches the type or if this value is an array and the
%% instance matches any type or schema in the array, then this instance
%% is not valid.
%% @private
check_disallow(Value, Disallow, JsonSchema) ->
  try check_type(Value, Disallow, []) of
      ok -> throw({data_invalid, Value, disallowed, JsonSchema})
  catch
    throw:{data_invalid, _, _, _} -> ok
  end.

%% @doc 5.26.  extends
%%
%% The value of this property MUST be another schema which will provide
%% a base schema which the current schema will inherit from.  The
%% inheritance rules are such that any instance that is valid according
%% to the current schema MUST be valid according to the referenced
%% schema.  This MAY also be an array, in which case, the instance MUST
%% be valid for all the schemas in the array.  A schema that extends
%% another schema MAY define additional attributes, constrain existing
%% attributes, or add other constraints.
%% @private
check_extends(Value, Extends) when is_list(Extends) ->
  lists:foreach( fun(SchemaKey) ->
                     check_extends(Value, SchemaKey)
                 end
               , Extends
               );
check_extends(Value, Extends) ->
  case is_json_object(Extends) of
    true  ->
      check_value(Value, unwrap(Extends), Extends);
    false ->
      %% TODO: implement handling of $ref
      ok
  end.

%%=============================================================================
%% @doc Returns `true' if given values (instance) are equal, otherwise `false'
%% is returned.
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%%
%%    are null; or
%%
%%    are booleans/numbers/strings and have the same value; or
%%
%%    are arrays, contains the same number of items, and each item in
%%    the array is equal to the corresponding item in the other array;
%%    or
%%
%%    are objects, contains the same property names, and each property
%%    in the object is equal to the corresponding property in the other
%%    object.
%% @private
is_equal(Value1, Value2) ->
  case is_json_object(Value1) andalso is_json_object(Value2) of
    true  -> compare_objects(Value1, Value2);
    false -> case is_list(Value1) andalso is_list(Value2) of
               true  -> compare_lists(Value1, Value2);
               false -> Value1 =:= Value2
             end
  end.

%% @private
compare_lists(Value1, Value2) ->
  case length(Value1) =:= length(Value2) of
    true  -> compare_elements(Value1, Value2);
    false -> false
  end.

%% @private
compare_elements(Value1, Value2) ->
  lists:all( fun({Element1, Element2}) ->
                 is_equal(Element1, Element2)
             end
           , lists:zip(Value1, Value2)
           ).

%% @private
compare_objects(Value1, Value2) ->
  case length(unwrap(Value1)) =:= length(unwrap(Value2)) of
    true  -> compare_properties(Value1, Value2);
    false -> false
  end.

%% @private
compare_properties(Value1, Value2) ->
  lists:all( fun({PropertyName1, PropertyValue1}) ->
                 case get_path(PropertyName1, Value2) of
                   []             -> false;
                   PropertyValue2 -> is_equal(PropertyValue1, PropertyValue2)
                 end
             end
           , unwrap(Value1)
           ).

%%=============================================================================
%% @private
get_path(Key, Schema) ->
  jesse_json_path:path(Key, Schema).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
