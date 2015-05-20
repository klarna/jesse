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

%% @doc type
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

%% @doc properties
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

%% @doc items
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

%% @doc dependencies
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

%% @doc minimum
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

%%% @doc maximum
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

%% @doc minItems
%% @private
check_min_items(Value, MinItems, State) when length(Value) >= MinItems ->
  State;
check_min_items(Value, _MinItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc maxItems
%% @private
check_max_items(Value, MaxItems, State) when length(Value) =< MaxItems ->
  State;
check_max_items(Value, _MaxItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc uniqueItems
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

%% @doc pattern
%% @private
check_pattern(Value, Pattern, State) ->
  case re:run(Value, Pattern, [{capture, none}, unicode]) of
    match   -> State;
    nomatch ->
      handle_data_invalid(?no_match, Value, State)
  end.

%% @doc minLength
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc maxLength
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc enum
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

check_format(_Value, _Format, State) ->
  State.

%% @doc multipleOf
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

%% @doc required
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

%% @doc maxProperties
%% @private
check_max_properties(Value, MaxProperties, State)
  when is_integer(MaxProperties), MaxProperties >= 0 ->
    case length(unwrap(Value)) =< MaxProperties of
      true  -> State;
      false -> handle_data_invalid(?too_many_properties, Value, State)
    end;
check_max_properties(_Value, _MaxProperties, State) ->
    handle_schema_invalid(?wrong_max_properties, State).

%% @doc minProperties
%% @private
check_min_properties(Value, MinProperties, State)
  when is_integer(MinProperties), MinProperties >= 0 ->
    case length(unwrap(Value)) >= MinProperties of
      true  -> State;
      false -> handle_data_invalid(?too_few_properties, Value, State)
    end;
check_min_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_min_properties, State).

%% @doc allOf
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

%% @doc anyOf
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

%% @doc oneOf
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


%% @doc not
%% @private
check_not(Value, Schema, State) ->
  case validate_schema(Value, Schema, State) of
    {true, _}  -> handle_data_invalid(?not_schema_valid, Value, State);
    {false, _} -> State
  end.

validate_schema(Value, Schema, State0) ->
  try
    case jesse_lib:is_json_object(Schema) of
      true ->
        State1 = jesse_state:set_current_schema(Schema, State0),
        State2 = jesse_schema_validator:validate_with_state(Value, Schema, State1),
        {true, State2};
      false ->
        handle_schema_invalid(?invalid_schema, State0)
    end
  catch
    throw:Errors -> {false, Errors}
  end.

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
