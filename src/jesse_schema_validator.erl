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

%% @doc to be written
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

%% @doc to be written
%% @private
check_properties(Value, Properties) ->
  lists:foreach( fun({PropertyName, PropertySchema}) ->
                     case get_path(PropertyName, Value) of
                       [] ->
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

%% @doc
%% @private
check_pattern_properties(Value, PatternProperties) ->
  [ check_match({PropertyName, PropertyValue}, {Pattern, Schema})
   || {Pattern, Schema} <- unwrap(PatternProperties),
      {PropertyName, PropertyValue} <- unwrap(Value)],
  ok.

check_match({PropertyName, PropertyValue}, {Pattern, Schema}) ->
  case re:run(PropertyName, Pattern, [{capture, none}]) of
    match   -> check_value(PropertyValue, unwrap(Schema), Schema);
    nomatch -> ok
  end.

%% @doc to be written
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

filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case re:run(ExtraName, Pattern, [{capture, none}]) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc
%% @private
check_items(Value, Items, JsonSchema) when is_list(Items) ->
  Tuples = case length(Value) - length(Items) of
             0 ->
               lists:zip(Value, Items);
             NExtra when NExtra > 0 ->
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

%% @doc
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

%% @doc
%% @private
check_minimum(Value, Minimum, ExclusiveMinimum) ->
  Result = case ExclusiveMinimum of
             true  -> Value > Minimum;
             _     -> Value >= Minimum
           end,
  case Result of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_in_range
                   , {{minimum, Minimum}, {exclusive, ExclusiveMinimum}}
                   })
  end.

%%% @doc
%% @private
check_maximum(Value, Maximum, ExclusiveMaximum) ->
  Result = case ExclusiveMaximum of
             true  -> Value < Maximum;
             _     -> Value =< Maximum
           end,
  case Result of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_in_range
                   , {{maximum, Maximum}, {exclusive, ExclusiveMaximum}}
                   })
  end.

%% @doc
%% @private
check_min_items(Value, MinItems) when length(Value) >= MinItems ->
  ok;
check_min_items(Value, MinItems) ->
  throw({ data_invalid
        , Value
        , not_correct_size
        , {min_items, MinItems}
        }).

%% @doc
%% @private
check_max_items(Value, MaxItems) when length(Value) =< MaxItems ->
  ok;
check_max_items(Value, MaxItems) ->
  throw({ data_invalid
        , Value
        , not_correct_size
        , {max_items, MaxItems}
        }).

%% @doc
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

%% @doc
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

%% @doc
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

%% @doc
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

%% @doc
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

%% @doc
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

%% @doc
%% @private
check_disallow(Value, Disallow, JsonSchema) ->
  try check_type(Value, Disallow, []) of
      ok -> throw({data_invalid, Value, disallowed, JsonSchema})
  catch
    throw:{data_invalid, _, _, _} -> ok
  end.

%% @doc
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
%% @doc
%% private
is_equal(Value1, Value2) ->
  case is_json_object(Value1) andalso is_json_object(Value2) of
    true  -> compare_objects(Value1, Value2);
    false -> case is_list(Value1) andalso is_list(Value2) of
               true  -> compare_lists(Value1, Value2);
               false -> Value1 =:= Value2
             end
  end.

compare_lists(Value1, Value2) ->
  case length(Value1) =:= length(Value2) of
    true  -> compare_elements(Value1, Value2);
    false -> false
  end.

compare_elements(Value1, Value2) ->
  lists:all( fun({Element1, Element2}) ->
                 is_equal(Element1, Element2)
             end
           , lists:zip(Value1, Value2)
           ).

compare_objects(Value1, Value2) ->
  case length(unwrap(Value1)) =:= length(unwrap(Value2)) of
    true  -> compare_properties(Value1, Value2);
    false -> false
  end.

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
