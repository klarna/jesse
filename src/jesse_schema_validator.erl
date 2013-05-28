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
%% @author Andrey Latnikov <alatnikov@alertlogic.com>
%%
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%%
%% All restrictions are applied one by one, following the unspecified order,
%% since the standard does not provide restrictions on it. A validation result
%% is either a valid Json or a list of errors accumulated with the external
%% callback. In case of strict performance requirements it is recommended to use
%% fast-fail callback throwing an exception on the first error to stop
%% validation algorithm, otherwise it will continue up to the end.
%%
%% An error record is a tuple of the following format
%% <code>
%%    { 'schema_invalid', Schema :: json_term(), Error } |
%%    { 'data_invalid',   Schema :: json_term(), Error, Data :: json_term() }
%%
%%    Error = { 'missing_id_field', Field :: binary() }
%%          | { 'missing_required_property', Name :: binary() }
%%          | { 'missing_dependency', Name :: binary() }
%%          | { 'no_match', Pattern :: binary() }
%%          |   'no_extra_properties_allowed'
%%          |   'no_extra_items_allowed'
%%          |   'not_allowed'
%%          | { 'not_unique', Value :: json_term() }
%%          |   'not_in_range'
%%          |   'not_divisible'
%%          |   'wrong_type'
%%          | { 'wrong_type_items', Items :: json_term() }
%%          | { 'wrong_type_dependency', Dependency :: json_term() }
%%          |   'wrong_size'
%%          |   'wrong_length'
%%          |   'wrong_format'
%% </code>
%%
%% @end
%%%=============================================================================

-module(jesse_schema_validator).

%% API
-export([ validate/3
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
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).
-define(EXTENDS,              <<"extends">>).
-define(ID,                   <<"id">>).
-define(_REF,                 <<"$ref">>).                 % NOT IMPLEMENTED YET

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
-spec validate( JsonSchema  :: jesse:json_term()
              , Data        :: jesse:json_term()
              , AccTuple    :: {jesse:accumulator(), Arg0 :: term()}
              ) -> {ok, jesse:json_term()}
                 | {error, Arg1 :: term()}.
validate(JsonSchema, Value, AccTuple) ->
    case check_value(Value, unwrap(JsonSchema), JsonSchema, {ok, AccTuple}) of
        {ok, _}                 -> {ok, Value};
        {error, {_Fun, Errors}} -> {error, Errors}
    end.

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
%% 1) mochijson2 format (`{struct, proplist()}')
%% 2) jiffy format (`{proplist()}')
%% 3) jsx format (`[{binary() | atom(), any()}]')
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(any()) -> boolean().
is_json_object({struct, Value}) when is_list(Value) -> true;
is_json_object({Value}) when is_list(Value)         -> true;
%% handle `jsx' empty objects
is_json_object([{}])                                -> true;
%% very naive check. checks only the first element.
is_json_object([{Key, _Value} | _])
  when is_binary(Key) orelse is_atom(Key)
       andalso Key =/= struct                       -> true;
is_json_object(_)                                   -> false.

is_null(null) -> true;
is_null(_) -> false.

%%% Internal functions
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
%% @private
check_value(Value, [{?TYPE, Type} | Attrs], JsonSchema, Accumulator) ->
  case check_type(Value, Type) of
      true -> check_value(Value, Attrs, JsonSchema, Accumulator);
      false ->
          %% In case of incorrect type, no other properties are checked against
          %% this value, since it may not be safe
          Error = {'data_invalid', JsonSchema, 'wrong_type', Value},
          accumulate_error(Error, Accumulator)
  end;
check_value( Value
           , [{?PROPERTIES, Properties} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case is_json_object(Value) of
            true  -> check_properties(Value, unwrap(Properties),
                                      JsonSchema, Accumulator0);
            false -> Accumulator0
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?PATTERNPROPERTIES, PatternProperties} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case is_json_object(Value) of
            true  -> check_pattern_properties(Value, PatternProperties,
                                              Accumulator0);
            false -> Accumulator0
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?ADDITIONALPROPERTIES, AdditionalProperties} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case is_json_object(Value) of
            true  -> check_additional_properties(Value, AdditionalProperties,
                                                 JsonSchema, Accumulator0);
            false -> Accumulator0
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?ITEMS, Items} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case is_array(Value) of
            true  -> check_items(Value, Items, JsonSchema, Accumulator0);
            false -> Accumulator0
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
           , JsonSchema
           , Accumulator
           ) ->
    check_value(Value, Attrs, JsonSchema, Accumulator);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(Value, [{?REQUIRED, _Required} | Attrs], JsonSchema, Accumulator) ->
    check_value(Value, Attrs, JsonSchema, Accumulator);
check_value( Value
           , [{?DEPENDENCIES, Dependencies} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case is_json_object(Value) of
            true -> check_dependencies(Value, Dependencies,
                                       JsonSchema, Accumulator0);
            false -> Accumulator0
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?MINIMUM, Minimum} | Attrs], JsonSchema, Accumulator0) ->
    Exclusive = get_path(?EXCLUSIVEMINIMUM, JsonSchema),
    Accumulator1 =
        case check_minimum(Value, Minimum, Exclusive) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'not_in_range', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?MAXIMUM, Maximum} | Attrs], JsonSchema, Accumulator0) ->
    Exclusive = get_path(?EXCLUSIVEMAXIMUM, JsonSchema),
    Accumulator1 =
        case check_maximum(Value, Maximum, Exclusive) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'not_in_range', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMINIMUM, _ExclusiveMinimum} | Attrs]
           , JsonSchema
           , Accumulator
           ) ->
    check_value(Value, Attrs, JsonSchema, Accumulator);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMAXIMUM, _ExclusiveMaximum} | Attrs]
           , JsonSchema
           , Accumulator
           ) ->
    check_value(Value, Attrs, JsonSchema, Accumulator);
check_value(Value, [{?MINITEMS, MinItems} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case check_min_items(Value, MinItems) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'wrong_size', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case check_max_items(Value, MaxItems) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'wrong_size', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?UNIQUEITEMS, UniqueItems} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case is_array(Value) of
            false -> Accumulator0;
            true ->
                Error = check_unique_items(Value, UniqueItems, JsonSchema),
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?PATTERN, Pattern} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case check_pattern(Value, Pattern) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema,
                          {'no_match', Pattern}, Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?MINLENGTH, MinLength} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case check_min_length(Value, MinLength) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'wrong_length', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?MAXLENGTH, MaxLength} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case check_max_length(Value, MaxLength) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'wrong_length', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?ENUM, Enum} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case check_enum(Value, Enum) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'not_in_range', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?FORMAT, Format} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 =
        case check_format(Value, Format) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'wrong_format', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?DIVISIBLEBY, DivisibleBy} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case check_divisible_by(Value, DivisibleBy) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'not_divisible', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value( Value
           , [{?DISALLOW, Disallow} | Attrs]
           , JsonSchema
           , Accumulator0
           ) ->
    Accumulator1 =
        case check_disallow(Value, Disallow) of
            true -> Accumulator0;
            false ->
                Error = { 'data_invalid', JsonSchema, 'not_allowed', Value },
                accumulate_error(Error, Accumulator0)
        end,
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(Value, [{?EXTENDS, Extends} | Attrs], JsonSchema, Accumulator0) ->
    Accumulator1 = check_extends(Value, Extends, Accumulator0),
    check_value(Value, Attrs, JsonSchema, Accumulator1);
check_value(_Value, [], _JsonSchema, Accumulator) ->
    Accumulator;
check_value(Value, [_Attr | Attrs], JsonSchema, Accumulator) ->
    check_value(Value, Attrs, JsonSchema, Accumulator).

%% @doc 5.1.  type
%%
%% This attribute defines what the primitive type or the schema of the
%% instance MUST be in order to validate.  This attribute can take one
%% of two forms:
%% <dl>
%% <dt>Simple Types</dt>
%%  <dd>A string indicating a primitive or simple type. The
%%    following are acceptable string values:
%%    <dl>
%%    <dt>string</dt>  <dd>Value MUST be a string.</dd>
%%
%%    <dt>number</dt>  <dd>Value MUST be a number, floating point numbers are
%%       allowed.</dd>
%%
%%    <dt>integer</dt>  <dd>Value MUST be an integer, no floating point numbers
%%       are allowed.  This is a subset of the number type.</dd>
%%
%%    <dt>boolean</dt>  <dd>Value MUST be a boolean.</dd>
%%
%%    <dt>object</dt>  <dd>Value MUST be an object.</dd>
%%
%%    <dt>array</dt>  <dd>Value MUST be an array.</dd>
%%
%%    <dt>null</dt>  <dd>Value MUST be null.  Note this is mainly for purpose of
%%       being able use union types to define nullability.  If this type
%%       is not included in a union, null values are not allowed (the
%%       primitives listed above do not allow nulls on their own).</dd>
%%
%%    <dt>any</dt>  <dd>Value MAY be of any type including null.</dd>
%%
%%    If the property is not defined or is not in this list,
%%    then any type of value is acceptable.  Other type values MAY be used for
%%    custom purposes, but minimal validators of the specification
%%    implementation can allow any instance value on unknown type
%%    values.
%%    </dl>
%%  </dd>
%% <dt>Union Types</dt>
%%  <dd>An array of two or more simple type definitions.  Each
%%     item in the array MUST be a simple type definition or a schema.
%%     The instance value is valid if it is of the same type as one of
%%     the simple type definitions, or valid by one of the schemas, in
%%     the array.</dd>
%% </dl>
%%  For example, a schema that defines if an instance can be a string or
%%  a number would be:
%%
%%  {"type":["string","number"]}
%% @private
check_type(Value, ?STRING)  -> is_binary(Value);
check_type(Value, ?NUMBER)  -> is_number(Value);
check_type(Value, ?INTEGER) -> is_integer(Value);
check_type(Value, ?BOOLEAN) -> is_boolean(Value);
check_type(Value, ?OBJECT)  -> is_json_object(Value);
check_type(Value, ?ARRAY)   -> is_array(Value);
check_type(Value, ?NULL)    -> is_null(Value);
check_type(_Value, ?ANY)    -> true;
check_type(Value, UnionType) ->
  case is_array(UnionType) of
    true  -> check_union_type(Value, UnionType);
    false -> true
  end.

%% @private
check_union_type(Value, UnionType) ->
    lists:any(
        fun(Type) ->
            case is_json_object(Type) of
                true ->
                    %% case when there's a schema in the array,
                    %% then we need to validate against
                    %% that schema
                    Acc = {ok, {fun dummy_accumulator/2, undefined}},
                    Acc =:= check_value(Value, unwrap(Type), Type, Acc);
                false ->
                    true =:= check_type(Value, Type)
            end
        end,
        UnionType
    ).

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
check_properties(Value, Properties, Schema, Accumulator) ->
    FoldFun =
        fun({PropertyName, PropertySchema}, Acc0) ->
            case get_path(PropertyName, Value) of
                [] ->
                    %% @doc 5.7.  required
                    %%
                    %% This attribute indicates if the instance must have a
                    %% value, and not be undefined. This is false by default,
                    %% making the instance optional.
                    %% @end
                    case get_path(?REQUIRED, PropertySchema) of
                        true ->
                            Err = { 'data_invalid', Schema,
                                    {'missing_required_property', PropertyName},
                                    Value },
                            accumulate_error(Err, Acc0);

                        _ -> Acc0
                    end;

                Property ->
                    check_value(Property, unwrap(PropertySchema),
                                PropertySchema, Acc0)
            end
        end,
    lists:foldl(FoldFun, Accumulator, Properties).

%% @doc 5.3.  patternProperties
%%
%% This attribute is an object that defines the schema for a set of
%% property names of an object instance.  The name of each property of
%% this attribute's object is a regular expression pattern in the ECMA
%% 262/Perl 5 format, while the value is a schema.  If the pattern
%% matches the name of a property on the instance object, the value of
%% the instance's property MUST be valid against the pattern name's
%% schema value.
%% @private
check_pattern_properties(Value, PatternProperties, Accumulator) ->
    lists:foldl(
        fun (Pattern, Accumulator0) ->
            lists:foldl(
                fun(Property, Accumulator1) ->
                    check_match(Property, Pattern, Accumulator1)
                end,
                Accumulator0,
                unwrap(Value)
            )
        end,
        Accumulator,
        unwrap(PatternProperties)
    ).

%% @private
check_match({PropertyName, PropertyValue}, {Pattern, Schema}, Accumulator) ->
    case re:run(PropertyName, Pattern, [{capture, none}]) of
        match ->
            check_value(PropertyValue, unwrap(Schema), Schema, Accumulator);
        nomatch -> Accumulator
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
check_additional_properties(Value, false, JsonSchema, Accumulator) ->
    Properties        = get_path(?PROPERTIES, JsonSchema),
    PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
    case get_additional_properties(Value, Properties, PatternProperties) of
        [] -> Accumulator;
        _Extras ->
            Error = { 'data_invalid', JsonSchema,
                      'no_extra_properties_allowed', Value },
            accumulate_error(Error, Accumulator)
    end;
check_additional_properties(_Value, true, _JsonSchema, Accumulator) ->
    Accumulator;
check_additional_properties( Value
                           , AdditionalProperties
                           , JsonSchema
                           , Accumulator
                           ) ->
    Properties        = get_path(?PROPERTIES, JsonSchema),
    PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
    case get_additional_properties(Value, Properties, PatternProperties) of
        []     -> Accumulator;
        Extras ->
            lists:foldl(
                fun(Extra, Accumulator0) ->
                    check_value( Extra
                               , unwrap(AdditionalProperties)
                               , AdditionalProperties
                               , Accumulator0 )
                end,
                Accumulator,
                Extras
            )
    end.

%% @private
get_additional_properties(Value, Properties, PatternProperties) ->
    ValuePropertiesNames  = [Name || {Name, _} <- unwrap(Value)],
    SchemaPropertiesNames = [Name || {Name, _} <- unwrap(Properties)],
    Patterns    = [Pattern || {Pattern, _} <- unwrap(PatternProperties)],
    ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
    ExtraNames  = lists:foldl(
        fun(Pattern, ExtraAcc) -> filter_extra_names(Pattern, ExtraAcc) end
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
check_items(Value, Items, JsonSchema, Accumulator) ->
    case is_json_object(Items) of
        true  ->
            lists:foldl(
                fun(Item, Acc0) ->
                    check_value(Item, unwrap(Items), Items, Acc0)
                end,
                Accumulator,
                Value
            );

        false when is_list(Items) ->
            check_items_array(Value, Items, JsonSchema, Accumulator);

        _ ->
            Error = {'schema_invalid', JsonSchema, {'wrong_type_items', Items}},
            accumulate_error(Error, Accumulator)
    end.

%% @private
check_items_array(Value, Items, JsonSchema, Accumulator) ->
    CheckItemsFun =
        fun (Tuples) ->
            lists:foldl(
                fun({Item, Schema}, Acc0) ->
                    check_value(Item, unwrap(Schema), Schema, Acc0)
                end,
                Accumulator,
                Tuples
            )
        end,

    %% @doc 5.6.  additionalItems
    %%
    %% This provides a definition for additional items in an array instance
    %% when tuple definitions of the items is provided.  This can be false
    %% to indicate additional items in the array are not allowed, or it can
    %% be a schema that defines the schema of the additional items.
    %% @end
    case length(Value) - length(Items) of
        0 ->
            CheckItemsFun(lists:zip(Value, Items));
        NExtra when NExtra > 0 ->
            case get_path(?ADDITIONALITEMS, JsonSchema) of
                []      -> Accumulator;
                true    -> Accumulator;
                false   ->
                    accumulate_error({ 'data_invalid', JsonSchema,
                                       'no_extra_items_allowed', Value },
                                     Accumulator);
                AdditionalItems ->
                    ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
                    CheckItemsFun(
                        lists:zip(Value, lists:append(Items, ExtraSchemas))
                    )
            end;
        NExtra when NExtra < 0 ->
            accumulate_error({ 'data_invalid', JsonSchema,
                               'not_enought_items', Value }, Accumulator)
    end.

%% @doc 5.8.  dependencies
%%
%% This attribute is an object that defines the requirements of a
%% property on an instance object.  If an object instance has a property
%% with the same name as a property in this attribute's object, then the
%% instance must be valid against the attribute's property value
%% (hereafter referred to as the "dependency value").
%%
%% The dependency value can take one of two forms:
%% <dl>
%% <dt>Simple Dependency</dt>  <dd>If the dependency value is a string,
%%    then the instance object MUST have a property with the same name as the
%%    dependency value.  If the dependency value is an array of strings,
%%    then the instance object MUST have a property with the same name
%%    as each string in the dependency value's array.</dd>
%%
%% <dt>Schema Dependency</dt>  <dd>If the dependency value is a schema, then the
%%    instance object MUST be valid against the schema.</dd>
%% </dl>
%% @private
check_dependencies(Value, Dependencies, JsonSchema, Accumulator) ->
    lists:foldl(
        fun({DependencyName, DependencyValue}, Acc0) ->
            case get_path(DependencyName, Value) of
                [] -> Acc0;
                _  -> check_dependency_value(Value, DependencyValue,
                                             JsonSchema, Acc0)
            end
        end,
        Accumulator,
        unwrap(Dependencies)
    ).

%% @private
check_dependency_value(Value, Dependency, JsonSchema, Accumulator)
when is_binary(Dependency) ->
    case get_path(Dependency, Value) of
        [] -> accumulate_error({ 'data_invalid', JsonSchema,
                                 {'missing_dependency', Dependency}, Value },
                               Accumulator);
        _  -> Accumulator
    end;
check_dependency_value(Value, Dependency, JsonSchema, Accumulator) ->
    case is_json_object(Dependency) of
        true -> check_value(Value, unwrap(Dependency), Dependency, Accumulator);
        false ->
            case is_list(Dependency) of
                true -> check_dependency_array(Value, Dependency,
                                               JsonSchema, Accumulator);
                false ->
                    Error = { 'schema_invalid', JsonSchema,
                              {'wrong_type_dependency', Dependency} },
                    accumulate_error(Error, Accumulator)
            end
    end.

%% @private
check_dependency_array(Value, Dependency, JsonSchema, Accumulator) ->
    lists:foldl(
        fun(PropertyName, Acc0) ->
            check_dependency_value(Value, PropertyName, JsonSchema, Acc0)
        end,
        Accumulator,
        Dependency
    ).

%% @private
%% @doc 5.9.  minimum
%%
%% This attribute defines the minimum value of the instance property
%% when the type of the instance value is a number.
%%
%% 5.11.  exclusiveMinimum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "minimum" attribute.  This is false by default, meaning the instance
%% value can be greater then or equal to the minimum value.
%% @end
check_minimum(Value, Minimum, ExclusiveMinimum) when is_number(Value) ->
    case ExclusiveMinimum of
        true -> Value > Minimum;
        _    -> Value >= Minimum
    end;
check_minimum(_Value, _Minimum, _ExclusiveMinimum) -> true.

%% @private
%% @doc 5.10.  maximum
%%
%% This attribute defines the maximum value of the instance property
%% when the type of the instance value is a number.
%%
%% 5.12.  exclusiveMaximum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "maximum" attribute.  This is false by default, meaning the instance
%% value can be less then or equal to the maximum value.
%% @end
check_maximum(Value, Maximum, ExclusiveMaximum) when is_number(Value) ->
    case ExclusiveMaximum of
        true -> Value < Maximum;
        _    -> Value =< Maximum
    end;
check_maximum(_Value, _Maximum, _ExclusiveMaximum) -> true.

%% @doc 5.13.  minItems
%%
%% This attribute defines the minimum number of values in an array when
%% the array is the instance value.
%% @private
check_min_items(Value, MinItems) ->
    case is_array(Value) of
        true -> length(Value) >= MinItems;
        _    -> true
    end.

%% @doc 5.14.  maxItems
%%
%% This attribute defines the maximum number of values in an array when
%% the array is the instance value.
%% @private
check_max_items(Value, MaxItems)  ->
    case is_array(Value) of
        true -> length(Value) =< MaxItems;
        _    -> true
    end.

%% @doc 5.15.  uniqueItems
%%
%% This attribute indicates that all items in an array instance MUST be
%% unique (contains no two identical values).
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
%% @private
check_unique_items(Value, true, JsonSchema) ->
    try
        lists:foldl(
            fun (_Item, []) -> ok;
                (Item, RestItems) ->
                    lists:foreach(
                        fun(ItemFromRest) ->
                            case is_equal(Item, ItemFromRest) of
                                true  ->
                                    throw({'data_invalid', JsonSchema,
                                            {'not_unique', Item}, Value});
                                false -> ok
                            end
                        end
                        , RestItems
                    ),
                    tl(RestItems)
            end,
            tl(Value),
            Value
        )
    catch
        throw:Error -> Error
    end.

%% @doc 5.16.  pattern
%% When the instance value is a string, this provides a regular
%% expression that a string instance MUST match in order to be valid.
%% Regular expressions SHOULD follow the regular expression
%% specification from ECMA 262/Perl 5
%% @private
check_pattern(Value, Pattern) when is_binary(Value) ->
    case re:run(Value, Pattern, [{capture, none}]) of
        match   -> true;
        nomatch -> false
    end;
check_pattern(_Value, _Pattern) -> true.

%% @doc 5.17.  minLength
%%
%% When the instance value is a string, this defines the minimum length
%% of the string.
%% @private
check_min_length(Value, MinLength) when is_binary(Value) ->
    length(unicode:characters_to_list(Value)) >= MinLength;
check_min_length(_Value, _MinLength) -> true.

%% @doc 5.18.  maxLength
%%
%% When the instance value is a string, this defines the maximum length
%% of the string.
%% @private
check_max_length(Value, MaxLength) when is_binary(Value) ->
    length(unicode:characters_to_list(Value)) =< MaxLength;
check_max_length(_Value, _MaxLength) -> true.

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
    lists:any(fun(ExpectedValue) -> is_equal(Value, ExpectedValue) end, Enum).

%% TODO:
check_format(_Value, _Format) -> ok.

%% @doc 5.24.  divisibleBy
%%
%% This attribute defines what value the number instance must be
%% divisible by with no remainder (the result of the division must be an
%% integer.)  The value of this attribute SHOULD NOT be 0.
%% @private
check_divisible_by(_Value, 0) -> false;
check_divisible_by(Value, DivisibleBy)
when is_number(Value) andalso is_number(DivisibleBy) ->
    Result = (Value / DivisibleBy - trunc(Value / DivisibleBy)) * DivisibleBy,
    case Result of
        0.0 -> true;
        _   -> false
    end;
check_divisible_by(_Value, _DivisibleBy) -> true.

%% @doc 5.25.  disallow
%%
%% This attribute takes the same values as the "type" attribute, however
%% if the instance matches the type or if this value is an array and the
%% instance matches any type or schema in the array, then this instance
%% is not valid.
%% @private
check_disallow(Value, Disallow) ->
    case check_type(Value, Disallow) of
        true -> false;
        false -> true
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
check_extends(Value, Extends, Accumulator) ->
    case is_json_object(Extends) of
        true  ->
            check_value(Value, unwrap(Extends), Extends, Accumulator);
        false ->
            case is_list(Extends) of
                true  -> check_extends_array(Value, Extends, Accumulator);
                false -> ok %% TODO: implement handling of $ref
            end
    end.

%% @private
check_extends_array(Value, Extends, Accumulator) ->
    lists:foldl(
        fun(SchemaKey, Acc0) -> check_extends(Value, SchemaKey, Acc0) end,
        Accumulator,
        Extends
    ).

%%=============================================================================
%% @doc Returns `true' if given values (instance) are equal, otherwise `false'
%% is returned.
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
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

%% @private
accumulate_error(ok, Accumulator) -> Accumulator;
accumulate_error(Error, {_Result, {Fun, Acc0}}) ->
    {error, {Fun, Fun(Error, Acc0)}}.

dummy_accumulator(_Error, undefined) ->
    undefined.

%%=============================================================================
%% @doc This check is needed since objects in `jsx' are lists (proplists)
%% @private
is_array(Value) when is_list(Value) -> not is_json_object(Value);
is_array(_)                         -> false.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
