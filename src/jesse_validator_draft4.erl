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
%% according to the standard(draft4).
%% @end
%%%=============================================================================

-module(jesse_validator_draft4).

%% API
-export([ check_value/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%%% API
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
-spec check_value( Value      :: any()
                 , JsonSchema :: jesse:json_term()
                 , State      :: jesse_state:state()
                 ) -> jesse_state:state() | no_return().
check_value(Value, [{?TYPE, Type} | Attrs], State) ->
  NewState = check_type(Value, Type, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PROPERTIES, Properties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_properties( Value
                                        , unwrap(Properties)
                                        , State
                                        );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?PATTERNPROPERTIES, PatternProperties} | Attrs]
           , State
           ) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_pattern_properties( Value
                                                , PatternProperties
                                                , State
                                                );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?ADDITIONALPROPERTIES, AdditionalProperties} | Attrs]
           , State
           ) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_additional_properties( Value
                                                   , AdditionalProperties
                                                   , State
                                                   );
               false -> State
       end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ITEMS, Items} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_items(Value, Items, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?REQUIRED, Required} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_required(Value, Required, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DEPENDENCIES, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependencies(Value, Dependencies, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINIMUM, Minimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 ExclusiveMinimum = get_value( ?EXCLUSIVEMINIMUM
                                             , get_current_schema(State)
                                             ),
                 check_minimum(Value, Minimum, ExclusiveMinimum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXIMUM, Maximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 ExclusiveMaximum = get_value( ?EXCLUSIVEMAXIMUM
                                             , get_current_schema(State)
                                             ),
                 check_maximum(Value, Maximum, ExclusiveMaximum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMINIMUM, _ExclusiveMinimum} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?EXCLUSIVEMAXIMUM, _ExclusiveMaximum} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?MINITEMS, MinItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_min_items(Value, MinItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_max_items(Value, MaxItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_unique_items(Value, Uniqueitems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PATTERN, Pattern} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_pattern(Value, Pattern, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINLENGTH, MinLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_min_length(Value, MinLength, State);
               false -> State
  end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXLENGTH, MaxLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_max_length(Value, MaxLength, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ENUM, Enum} | Attrs], State) ->
  NewState = check_enum(Value, Enum, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?FORMAT, Format} | Attrs], State) ->
  NewState = check_format(Value, Format, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MULTIPLEOF, Multiple} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_multiple_of(Value, Multiple, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXPROPERTIES, MaxProperties} | Attrs], State) ->
    NewState = case jesse_lib:is_json_object(Value) of
                   true  -> check_max_properties(Value, MaxProperties, State);
                   false -> State
               end,
    check_value(Value, Attrs, NewState);
check_value(Value, [{?MINPROPERTIES, MinProperties} | Attrs], State) ->
    NewState = case jesse_lib:is_json_object(Value) of
                   true  -> check_min_properties(Value, MinProperties, State);
                   false -> State
               end,
    check_value(Value, Attrs, NewState);
check_value(Value, [{?ALLOF, Schemas} | Attrs], State) ->
    NewState = check_all_of(Value, Schemas, State),
    check_value(Value, Attrs, NewState);
check_value(Value, [{?ANYOF, Schemas} | Attrs], State) ->
    NewState = check_any_of(Value, Schemas, State),
    check_value(Value, Attrs, NewState);
check_value(Value, [{?ONEOF, Schemas} | Attrs], State) ->
    NewState = check_one_of(Value, Schemas, State),
    check_value(Value, Attrs, NewState);
check_value(Value, [{?NOT, Schema} | Attrs], State) ->
    NewState = check_not(Value, Schema, State),
    check_value(Value, Attrs, NewState);
check_value(Value, [{?_REF, Reference} | Attrs], State) ->
    NewState = resolve_ref(Value, Reference, State),
    check_value(Value, Attrs, NewState);
check_value(_Value, [], State) ->
  State;
check_value(Value, [_Attr | Attrs], State) ->
  check_value(Value, Attrs, State).

%%% Internal functions
%% @doc Adds Property to the current path and checks the value
%% using jesse_schema_validator:validate_with_state/3.
%% @private
check_value(Property, Value, Attrs, State) ->
  %% Add Property to path
  State1 = jesse_state:add_to_path(State, Property),
  State2 = jesse_schema_validator:validate_with_state(Attrs, Value, State1),
  %% Reset path again
  jesse_state:remove_last_from_path(State2).

%% @doc 5.5.2. type
%%
%% 5.5.2.1. Valid values
%%
%%   The value of this keyword MUST be either a string or an array. If it is an
%%   array, elements of the array MUST be strings and MUST be unique.
%%
%%   String values MUST be one of the seven primitive types defined by the core
%%   specification.
%%
%%  5.5.2.2. Conditions for successful validation
%%    An instance matches successfully if its primitive type is one of the types
%%    defined by keyword. Recall: "number" includes "integer".
%%
%% @private
check_type(Value, Type, State) ->
  try
    IsValid = case jesse_lib:is_array(Type) of
                true  -> check_union_type(Value, Type, State);
                false -> is_type_valid(Value, Type)
              end,
    case IsValid of
      true  -> State;
      false -> wrong_type(Value, State)
    end
  catch
    %% The schema was invalid
    error:function_clause ->
      handle_schema_invalid(?wrong_type_specification, State)
  end.


%% @private
is_type_valid(Value, ?STRING)  -> is_binary(Value);
is_type_valid(Value, ?NUMBER)  -> is_number(Value);
is_type_valid(Value, ?INTEGER) -> is_integer(Value);
is_type_valid(Value, ?BOOLEAN) -> is_boolean(Value);
is_type_valid(Value, ?OBJECT)  -> jesse_lib:is_json_object(Value);
is_type_valid(Value, ?ARRAY)   -> jesse_lib:is_array(Value);
is_type_valid(Value, ?NULL)    -> jesse_lib:is_null(Value).

%% @private
check_union_type(Value, [_ | _] = UnionType, _State) ->
  lists:any(fun(Type) -> is_type_valid(Value, Type) end, UnionType);
check_union_type(_Value, _InvalidTypes, State) ->
    handle_schema_invalid(?wrong_type_specification, State).

%% @private
wrong_type(Value, State) ->
  handle_data_invalid(?wrong_type, Value, State).

%% @doc 5.4.4. additionalProperties, properties and patternProperties
%%
%% 5.4.4.1. Valid values
%%
%%   The value of "additionalProperties" MUST be a boolean or an object. If it
%%   is an object, it MUST also be a valid JSON Schema.
%%
%%   The value of "properties" MUST be an object. Each value of this object MUST
%%   be an object, and each object MUST be a valid JSON Schema.
%%
%%   The value of "patternProperties" MUST be an object. Each property name of
%%   this object SHOULD be a valid regular expression, according to the ECMA 262
%%   regular expression dialect. Each property value of this object MUST be an
%%   object, and each object MUST be a valid JSON Schema.
%%
%% 5.4.4.2. Conditions for successful validation
%%
%%   Successful validation of an object instance against these three keywords
%%   depends on the value of "additionalProperties":
%%
%%     if its value is boolean true or a schema, validation succeeds;
%%
%%     if its value is boolean false, the algorithm to determine validation
%%     success is described below.
%%
%% 5.4.4.3. Default values
%%
%%   If either "properties" or "patternProperties" are absent, they can be
%%   considered present with an empty object as a value.
%%
%%   If "additionalProperties" is absent, it may be considered present with an
%%   empty schema as a value.
%%
%% 5.4.4.4. If "additionalProperties" has boolean value false
%%
%%   In this case, validation of the instance depends on the property set of
%%   "properties" and "patternProperties". In this section, the property names
%%   of "patternProperties" will be called regexes for convenience.
%%
%%   The first step is to collect the following sets:
%%
%%     s  - The property set of the instance to validate.
%%     p  - The property set from "properties".
%%     pp - The property set from "patternProperties".
%%
%%   Having collected these three sets, the process is as follows:
%%
%%     remove from "s" all elements of "p", if any;
%%
%%     for each regex in "pp", remove all elements of "s" which this regex
%%     matches.
%%
%%   Validation of the instance succeeds if, after these two steps, set "s" is
%%   empty.
%%
%% @private
check_properties(Value, Properties, State) ->
  TmpState
    = lists:foldl( fun({PropertyName, PropertySchema}, CurrentState) ->
                       case get_value(PropertyName, Value) of
                         ?not_found ->
                           CurrentState;
                         Property ->
                           NewState = set_current_schema( CurrentState
                                                        , PropertySchema
                                                        ),
                           check_value( PropertyName
                                      , Property
                                      , PropertySchema
                                      , NewState
                                      )
                       end
                   end
                 , State
                 , Properties
                 ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc patternProperties
%% See check_properties/3.
%% @private
check_pattern_properties(Value, PatternProperties, State) ->
  P1P2 = [{P1, P2} || P1 <- unwrap(Value), P2  <- unwrap(PatternProperties)],
  TmpState = lists:foldl( fun({Property, Pattern}, CurrentState) ->
                              check_match(Property, Pattern, CurrentState)
                          end
                        , State
                        , P1P2
                        ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @private
check_match({PropertyName, PropertyValue}, {Pattern, Schema}, State) ->
  case re:run(PropertyName, Pattern, [{capture, none}, unicode]) of
    match   ->
      check_value( PropertyName
                 , PropertyValue
                 , Schema
                 , set_current_schema(State, Schema)
                 );
    nomatch ->
      State
  end.

%% @doc additionalProperties
%% See check_properties/3.
%% @private
check_additional_properties(Value, false, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      lists:foldl( fun({Property, _}, State1) ->
                       State2
                         = handle_data_invalid( ?no_extra_properties_allowed
                                              , Value
                                              , add_to_path(State1, Property)
                                              ),
                       remove_last_from_path(State2)
                   end
                 , State
                 , Extras
                 )
  end;
check_additional_properties(_Value, true, State) ->
  State;
check_additional_properties(Value, AdditionalProperties, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      TmpState
        = lists:foldl( fun({ExtraName, Extra}, CurrentState) ->
                           NewState = set_current_schema( CurrentState
                                                        , AdditionalProperties
                                                        ),
                           check_value( ExtraName
                                      , Extra
                                      , AdditionalProperties
                                      , NewState
                                      )
                       end
                     , State
                     , Extras
                     ),
      set_current_schema(TmpState, JsonSchema)
  end.

%% @doc Returns the additional properties as a list of pairs containing the name
%% and the value of all properties not covered by Properties
%% or PatternProperties.
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
  lists:map(fun(Name) -> {Name, get_value(Name, Value)} end, ExtraNames).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case re:run(ExtraName, Pattern, [{capture, none}]) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc 5.3.1. additionalItems and items
%%
%% 5.3.1.1. Valid values
%%
%%   The value of "additionalItems" MUST be either a boolean or an object. If it
%%   is an object, this object MUST be a valid JSON Schema.
%%
%%   The value of "items" MUST be either an object or an array. If it is an
%%   object, this object MUST be a valid JSON Schema. If it is an array, items
%%   of this array MUST be objects, and each of these objects MUST be a valid
%%   JSON Schema.
%%
%% 5.3.1.2. Conditions for successful validation
%%
%%   Successful validation of an array instance with regards to these two
%%   keywords is determined as follows:
%%
%%     if "items" is not present, or its value is an object, validation of the
%%     instance always succeeds, regardless of the value of "additionalItems";
%%
%%     if the value of "additionalItems" is boolean value true or an object,
%%     validation of the instance always succeeds;
%%
%%     if the value of "additionalItems" is boolean value false and the value of
%%     "items" is an array, the instance is valid if its size is less than, or
%%     equal to, the size of "items".
%%
%% 5.3.1.4. Default values
%%
%%   If either keyword is absent, it may be considered present with an empty
%%   schema.
%%
%% @private
check_items(Value, Items, State) ->
  case jesse_lib:is_json_object(Items) of
    true ->
      {_, TmpState} = lists:foldl( fun(Item, {Index, CurrentState}) ->
                                       { Index + 1
                                       , check_value( Index
                                                    , Item
                                                    , Items
                                                    , CurrentState
                                                    )
                                       }
                                   end
                                 , {0, set_current_schema(State, Items)}
                                 , Value
                                 ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Items) ->
      check_items_array(Value, Items, State);
    _ ->
      handle_schema_invalid({?wrong_type_items, Items}, State)
  end.

%% @private
check_items_array(Value, Items, State) ->
  JsonSchema = get_current_schema(State),
  case length(Value) - length(Items) of
    0 ->
      check_items_fun(lists:zip(Value, Items), State);
    NExtra when NExtra > 0 ->
      case get_value(?ADDITIONALITEMS, JsonSchema) of
        ?not_found -> State;
        true       -> State;
        false      ->
          handle_data_invalid(?no_extra_items_allowed, Value, State);
        AdditionalItems ->
          ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
          Tuples = lists:zip(Value, lists:append(Items, ExtraSchemas)),
          check_items_fun(Tuples, State)
      end;
    NExtra when NExtra < 0 ->
      handle_data_invalid(?not_enought_items, Value, State)
  end.

%% @private
check_items_fun(Tuples, State) ->
  {_, TmpState} = lists:foldl( fun({Item, Schema}, {Index, CurrentState}) ->
                                 NewState = set_current_schema( CurrentState
                                                              , Schema
                                                              ),
                                 { Index + 1
                                 , check_value(Index, Item, Schema, NewState)
                                 }
                               end
                             , {0, State}
                             , Tuples
                             ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc 5.4.5.  dependencies
%%
%% 5.4.5.1. Valid values
%%
%%   This keyword's value MUST be an object. Each value of this object MUST be
%%   either an object or an array.
%%
%%   If the value is an object, it MUST be a valid JSON Schema. This is called a
%%   schema dependency.
%%
%%   If the value is an array, it MUST have at least one element. Each element
%%   MUST be a string, and elements in the array MUST be unique. This is called
%%   a property dependency.
%%
%% 5.4.5.2. Conditions for successful validation
%%
%%   5.4.5.2.1. Schema dependencies
%%
%%     For all (name, schema) pair of schema dependencies, if the instance has a
%%     property by this name, then it must also validate successfully against
%%     the schema.
%%
%%     Note that this is the instance itself which must validate successfully,
%%     not the value associated with the property name.
%%
%%   5.4.5.2.2. Property dependencies
%%
%%     For each (name, propertyset) pair of property dependencies, if the
%%     instance has a property by this name, then it must also have properties
%%     with the same names as propertyset.
%%
%% @private
check_dependencies(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, DependencyValue}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> check_dependency_value( Value
                                                         , DependencyName
                                                         , DependencyValue
                                                         , CurrentState
                                                         )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @private
check_dependency_value(Value, DependencyName, Dependency, State) ->
  case jesse_lib:is_json_object(Dependency) of
    true ->
      TmpState = check_value( DependencyName
                            , Value
                            , Dependency
                            , set_current_schema(State, Dependency)
                            ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Dependency) ->
      check_dependency_array(Value, DependencyName, Dependency, State);
    _ ->
      handle_schema_invalid({?wrong_type_dependency, Dependency}, State)
  end.

check_dependency(Value, Dependency, State)
  when is_binary(Dependency) ->
  case get_value(Dependency, Value) of
    ?not_found ->
      handle_data_invalid({?missing_dependency, Dependency}, Value, State);
    _          ->
      State
  end;
check_dependency(_Value, _Dependency, State) ->
    handle_schema_invalid(?invalid_dependency, State).

%% @private
check_dependency_array(Value, DependencyName, Dependency, State) ->
  lists:foldl( fun(PropertyName, CurrentState) ->
                   case get_value(DependencyName, Value) of
                       ?not_found ->
                         CurrentState;
                       _Exists ->
                         check_dependency( Value
                                         , PropertyName
                                         , CurrentState
                                         )
                   end
               end
             , State
             , Dependency
             ).

%% @doc 5.1.3. minimum and exclusiveMinimum
%%
%% 5.1.3.1. Valid values
%%
%%   The value of "minimum" MUST be a JSON number. The value of
%%   "exclusiveMinimum" MUST be a boolean.
%%
%%   If "exclusiveMinimum" is present, "minimum" MUST also be present.
%%
%% 5.1.3.2. Conditions for successful validation
%%
%%   Successful validation depends on the presence and value of
%%   "exclusiveMinimum":
%%
%%     if "exclusiveMinimum" is not present, or has boolean value false, then
%%     the instance is valid if it is greater than, or equal to, the value of
%%     "minimum";
%%
%%     if "exclusiveMinimum" is present and has boolean value true, the instance
%%     is valid if it is strictly greater than the value of "minimum".
%%
%% 5.1.3.3. Default value
%%
%%   "exclusiveMinimum", if absent, may be considered as being present with
%%   boolean value false.
%%
%% @private
check_minimum(Value, Minimum, ExclusiveMinimum, State) ->
  Result = case ExclusiveMinimum of
             true -> Value > Minimum;
             _    -> Value >= Minimum
           end,
  case Result of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

%% @doc 5.1.2. maximum and exclusiveMaximum
%%
%% 5.1.2.1. Valid values
%%
%%   The value of "maximum" MUST be a JSON number. The value of
%%   "exclusiveMaximum" MUST be a boolean.
%%
%%   If "exclusiveMaximum" is present, "maximum" MUST also be present.
%%
%% 5.1.2.2. Conditions for successful validation
%%
%%   Successful validation depends on the presence and value of
%%   "exclusiveMaximum":
%%
%%     if "exclusiveMaximum" is not present, or has boolean value false, then
%%     the instance is valid if it is lower than, or equal to, the value of
%%     "maximum";
%%
%%     if "exclusiveMaximum" has boolean value true, the instance is valid if it
%%     is strictly lower than the value of "maximum".
%%
%% 5.1.2.3. Default value
%%
%%   "exclusiveMaximum", if absent, may be considered as being present with
%%   boolean value false.
%%
%% @private
check_maximum(Value, Maximum, ExclusiveMaximum, State) ->
  Result = case ExclusiveMaximum of
             true -> Value < Maximum;
             _    -> Value =< Maximum
           end,
  case Result of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

%% @doc 5.3.3.  minItems
%%
%% 5.3.3.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.3.3.2.  Conditions for successful validation
%%
%%   An array instance is valid against "minItems" if its size is greater than,
%%   or equal to, the value of this keyword.
%%
%% 5.3.3.3. Default value
%%
%%   If this keyword is not present, it may be considered present with a value
%%   of 0.
%%
%% @private
check_min_items(Value, MinItems, State) when length(Value) >= MinItems ->
  State;
check_min_items(Value, _MinItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc 5.3.2. maxItems
%%
%% 5.3.2.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.3.2.2. Conditions for successful validation
%%
%%   An array instance is valid against "maxItems" if its size is less than, or
%%   equal to, the value of this keyword.
%%
%% @private
check_max_items(Value, MaxItems, State) when length(Value) =< MaxItems ->
  State;
check_max_items(Value, _MaxItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc 5.3.4. uniqueItems
%%
%% 5.3.4.1. Valid values
%%
%%   The value of this keyword MUST be a boolean.
%%
%% 5.3.4.2. Conditions for successful validation
%%
%%   If this keyword has boolean value false, the instance validates
%%   successfully. If it has boolean value true, the instance validates
%%   successfully if all of its elements are unique.
%%
%% 5.3.4.3. Default value
%%
%%   If not present, this keyword may be considered present with boolean value
%%   false.
%%
%% @private
check_unique_items([], true, State) ->
    State;
check_unique_items(Value, true, State) ->
  try
    lists:foldl( fun(_Item, []) ->
                     ok;
                    (Item, RestItems) ->
                     lists:foreach( fun(ItemFromRest) ->
                                        case is_equal(Item, ItemFromRest) of
                                          true  ->
                                            throw({?not_unique, Item});
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
    State
  catch
    throw:ErrorInfo -> handle_data_invalid(ErrorInfo, Value, State)
  end.

%% @doc 5.2.3. pattern
%%
%% 5.2.3.1. Valid values
%%
%%   The value of this keyword MUST be a string. This string SHOULD be a valid
%%   regular expression, according to the ECMA 262 regular expression dialect.
%%
%% 5.2.3.2. Conditions for successful validation
%%
%%   A string instance is considered valid if the regular expression matches
%%   the instance successfully. Recall: regular expressions are not implicitly
%%   anchored.
%% @private
check_pattern(Value, Pattern, State) ->
  case re:run(Value, Pattern, [{capture, none}, unicode]) of
    match   -> State;
    nomatch ->
      handle_data_invalid(?no_match, Value, State)
  end.

%% @doc 5.2.2.  minLength
%%
%% 5.2.2.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.2.2.2. Conditions for successful validation
%%
%%   A string instance is valid against this keyword if its length is greater
%%   than, or equal to, the value of this keyword.
%%
%%   The length of a string instance is defined as the number of its characters
%%   as defined by RFC 4627 [RFC4627].
%%
%% 5.2.2.3. Default value
%%
%%   "minLength", if absent, may be considered as being present with integer
%%   value 0.
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc 5.2.1. maxLength
%%
%% 5.2.1.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.2.1.2. Conditions for successful validation
%%
%%   A string instance is valid against this keyword if its length is less than,
%%   or equal to, the value of this keyword.
%%
%%   The length of a string instance is defined as the number of its characters
%%   as defined by RFC 4627 [RFC4627].
%%
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc 5.5.1. enum
%%
%% 5.5.1.1. Valid values
%%
%%   The value of this keyword MUST be an array. This array MUST have at least
%%   one element. Elements in the array MUST be unique.
%%
%%   Elements in the array MAY be of any type, including null.
%%
%% 5.5.1.2. Conditions for successful validation
%%
%%   An instance validates successfully against this keyword if its value is
%%   equal to one of the elements in this keyword's array value.
%%
%% @private
check_enum(Value, Enum, State) ->
  IsValid = lists:any( fun(ExpectedValue) ->
                           is_equal(Value, ExpectedValue)
                       end
                     , Enum
                     ),
  case IsValid of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

%% @doc format
%% Used for semantic validation.
%% TODO: Implement the standard formats
%% @private
check_format(_Value, _Format, State) ->
  State.

%% @doc 5.1.1. multipleOf
%%
%% 5.1.1.1. Valid values
%%
%%   The value of "multipleOf" MUST be a JSON number. This number MUST be
%%   strictly greater than 0.
%%
%% 5.1.1.2. Conditions for successful validation
%%
%%   A numeric instance is valid against "multipleOf" if the result of the
%%   division of the instance by this keyword's value is an integer.
%%
%% @private
check_multiple_of(Value, MultipleOf, State)
  when is_number(MultipleOf), MultipleOf > 0 ->
  Result = (Value / MultipleOf - trunc(Value / MultipleOf)) * MultipleOf,
  case Result of
    0.0 ->
      State;
    _   ->
      handle_data_invalid(?not_multiple_of, Value, State)
  end;
check_multiple_of(_Value, _MultipleOf, State) ->
  handle_schema_invalid(?wrong_multiple_of, State).

%% @doc 5.4.3. required
%%
%% 5.4.3.1. Valid values
%%
%%   The value of this keyword MUST be an array. This array MUST have at least
%%   one element. Elements of this array MUST be strings, and MUST be unique.
%%
%% 5.4.3.2. Conditions for successful validation
%%
%%   An object instance is valid against this keyword if its property set
%%   contains all elements in this keyword's array value.
%%
%% @private
check_required(Value, [_ | _] = Required, State) ->
  IsValid = lists:all( fun(PropertyName) ->
                           get_value(PropertyName, Value) =/= ?not_found
                       end
                     , Required
                     ),
    case IsValid of
      true  -> State;
      false -> handle_data_invalid(?missing_required_property, Value, State)
    end;
check_required(_Value, _InvalidRequired, State) ->
    handle_schema_invalid(?wrong_required_array, State).

%% @doc 5.4.1. maxProperties
%%
%% 5.4.1.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.4.1.2.Conditions for successful validation
%%
%%   An object instance is valid against "maxProperties" if its number of
%%   properties is less than, or equal to, the value of this keyword.
%%
%% @private
check_max_properties(Value, MaxProperties, State)
  when is_integer(MaxProperties), MaxProperties >= 0 ->
    case length(unwrap(Value)) =< MaxProperties of
      true  -> State;
      false -> handle_data_invalid(?too_many_properties, Value, State)
    end;
check_max_properties(_Value, _MaxProperties, State) ->
    handle_schema_invalid(?wrong_max_properties, State).

%% @doc 5.4.2. minProperties
%%
%% 5.4.2.1. Valid values
%%
%%   The value of this keyword MUST be an integer. This integer MUST be greater
%%   than, or equal to, 0.
%%
%% 5.4.2.2. Conditions for successful validation
%%
%%   An object instance is valid against "minProperties" if its number of
%%   properties is greater than, or equal to, the value of this keyword.
%%
%% 5.4.2.3. Default value
%%
%%   If this keyword is not present, it may be considered present with a value
%%   of 0.
%%
%% @private
check_min_properties(Value, MinProperties, State)
  when is_integer(MinProperties), MinProperties >= 0 ->
    case length(unwrap(Value)) >= MinProperties of
      true  -> State;
      false -> handle_data_invalid(?too_few_properties, Value, State)
    end;
check_min_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_min_properties, State).

%% @doc 5.5.3. allOf
%%
%% 5.5.3.1. Valid values
%%
%%   This keyword's value MUST be an array. This array MUST have at least one
%%   element.
%%
%%   Elements of the array MUST be objects. Each object MUST be a valid JSON
%%   Schema.
%%
%% 5.5.3.2. Conditions for successful validation
%%
%%   An instance validates successfully against this keyword if it validates
%%   successfully against all schemas defined by this keyword's value.
%%
%% @private
check_all_of(Value, [_ | _] = Schemas, State) ->
  check_all_of_(Value, Schemas, State);
check_all_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_all_of_schema_array, State).

check_all_of_(_Value, [], State) ->
    State;
check_all_of_(Value, [Schema | Schemas], State) ->
  case validate_schema(Value, Schema, State) of
    {true, NewState} -> check_all_of_(Value, Schemas, NewState);
    {false, _} -> handle_data_invalid(?all_schemas_not_valid, Value, State)
  end.

%% @doc 5.5.4. anyOf
%%
%% 5.5.4.1. Valid values
%%
%%   This keyword's value MUST be an array. This array MUST have at least one
%%   element.
%%
%%   Elements of the array MUST be objects. Each object MUST be a valid JSON
%%   Schema.
%%
%% 5.5.4.2. Conditions for successful validation
%%
%%   An instance validates successfully against this keyword if it validates
%%   successfully against at least one schema defined by this keyword's value.
%%
%% @private
check_any_of(Value, [_ | _] = Schemas, State) ->
  check_any_of_(Value, Schemas, State);
check_any_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_any_of_schema_array, State).

check_any_of_(Value, [], State) ->
  handle_data_invalid(?any_schemas_not_valid, Value, State);
check_any_of_(Value, [Schema | Schemas], State) ->
  case validate_schema(Value, Schema, State) of
    {true, NewState} -> NewState;
    {false, _} -> check_any_of_(Value, Schemas, State)
  end.

%% @doc 5.5.5. oneOf
%%
%% 5.5.5.1. Valid values
%%
%%   This keyword's value MUST be an array. This array MUST have at least one
%%   element.
%%
%%   Elements of the array MUST be objects. Each object MUST be a valid JSON
%%   Schema.
%%
%% 5.5.5.2. Conditions for successful validation
%%
%%   An instance validates successfully against this keyword if it validates
%%   successfully against exactly one schema defined by this keyword's value.
%%
%% @private
check_one_of(Value, [_ | _] = Schemas, State) ->
  check_one_of_(Value, Schemas, State, 0);
check_one_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_one_of_schema_array, State).

check_one_of_(_Value, [], State, 1) ->
  State;
check_one_of_(Value, [], State, 0) ->
  handle_data_invalid(?not_one_schema_valid, Value, State);
check_one_of_(Value, _Schemas, State, Valid) when Valid > 1 ->
  handle_data_invalid(?not_one_schema_valid, Value, State);
check_one_of_(Value, [Schema | Schemas], State, Valid) ->
  case validate_schema(Value, Schema, State) of
    {true, NewState} ->
      check_one_of_(Value, Schemas, NewState, Valid + 1);
    {false, _} ->
      check_one_of_(Value, Schemas, State, Valid)
  end.


%% @doc 5.5.6. not
%%
%% 5.5.6.1. Valid values
%%
%%   This keyword's value MUST be an object. This object MUST be a valid JSON
%%   Schema.
%%
%% 5.5.6.2. Conditions for successful validation
%%
%%   An instance is valid against this keyword if it fails to validate
%%   successfully against the schema defined by this keyword.
%%
%% @private
check_not(Value, Schema, State) ->
  case validate_schema(Value, Schema, State) of
    {true, _}  -> handle_data_invalid(?not_schema_valid, Value, State);
    {false, _} -> State
  end.

%% @doc Validate a value against a schema in a given state.
%% Used by all combinators to run validation on a schema.
%% @private
validate_schema(Value, Schema, State0) ->
  try
    case jesse_lib:is_json_object(Schema) of
      true ->
        State1 = jesse_state:set_current_schema(State0, Schema),
        State2 = jesse_schema_validator:validate_with_state(Schema, Value, State1),
        {true, State2};
      false ->
        handle_schema_invalid(?schema_invalid, State0)
    end
  catch
    throw:Errors -> {false, Errors}
  end.

%% @doc Resolve a JSON reference
%% The "id" keyword is taken care of behind the scenes in jesse_state.
%% @private
resolve_ref(Value, Reference, State) ->
  NewState = jesse_state:resolve_reference(State, Reference),
  Schema = get_current_schema(NewState),
  jesse_schema_validator:validate_with_state(Schema, Value, NewState).


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
  case jesse_lib:is_json_object(Value1)
    andalso jesse_lib:is_json_object(Value2) of
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
                 case get_value(PropertyName1, Value2) of
                   ?not_found     -> false;
                   PropertyValue2 -> is_equal(PropertyValue1, PropertyValue2)
                 end
             end
           , unwrap(Value1)
           ).

%%=============================================================================
%% Wrappers
%% @private
get_value(Key, Schema) ->
  jesse_json_path:value(Key, Schema, ?not_found).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%% @private
handle_data_invalid(Info, Value, State) ->
  jesse_error:handle_data_invalid(Info, Value, State).

%% @private
handle_schema_invalid(Info, State) ->
  jesse_error:handle_schema_invalid(Info, State).

%% @private
get_current_schema(State) ->
  jesse_state:get_current_schema(State).

%% @private
set_current_schema(State, NewSchema) ->
  jesse_state:set_current_schema(State, NewSchema).

%% @private
empty_if_not_found(Value) ->
  jesse_lib:empty_if_not_found(Value).

%% @private
add_to_path(State, Property) ->
  jesse_state:add_to_path(State, Property).

%% @private
remove_last_from_path(State) ->
  jesse_state:remove_last_from_path(State).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
