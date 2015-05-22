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

%% Constant definitions for Json schema keywords
-define(_SCHEMA,              <<"$schema">>).
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
-define(_REF,                 <<"$ref">>).
-define(ALLOF,                <<"allOf">>).
-define(ANYOF,                <<"anyOf">>).
-define(ONEOF,                <<"oneOf">>).
-define(NOT,                  <<"not">>).
-define(MULTIPLEOF,           <<"multipleOf">>).
-define(MAXPROPERTIES,        <<"maxProperties">>).
-define(MINPROPERTIES,        <<"minProperties">>).

%% Constant definitions for Json types
-define(ANY,                  <<"any">>).
-define(ARRAY,                <<"array">>).
-define(BOOLEAN,              <<"boolean">>).
-define(INTEGER,              <<"integer">>).
-define(NULL,                 <<"null">>).
-define(NUMBER,               <<"number">>).
-define(OBJECT,               <<"object">>).
-define(STRING,               <<"string">>).

%% Supported $schema attributes
-define(json_schema_draft3, <<"http://json-schema.org/draft-03/schema#">>).
-define(json_schema_draft4, <<"http://json-schema.org/draft-04/schema#">>).
-define(default_schema_ver, ?json_schema_draft3).

%%
-define(data_invalid,                'data_invalid').
-define(schema_invalid,              'schema_invalid').

-define(missing_id_field,            'missing_id_field').
-define(missing_required_property,   'missing_required_property').
-define(missing_dependency,          'missing_dependency').
-define(no_match,                    'no_match').
-define(no_extra_properties_allowed, 'no_extra_properties_allowed').
-define(no_extra_items_allowed,      'no_extra_items_allowed').
-define(not_enought_items,           'not_enought_items').
-define(not_allowed,                 'not_allowed').
-define(not_unique,                  'not_unique').
-define(not_in_range,                'not_in_range').
-define(not_divisible,               'not_divisible').
-define(wrong_type,                  'wrong_type').
-define(wrong_type_items,            'wrong_type_items').
-define(wrong_type_dependency,       'wrong_type_dependency').
-define(invalid_dependency,          'invalid_dependency').
-define(wrong_size,                  'wrong_size').
-define(wrong_length,                'wrong_length').
-define(wrong_format,                'wrong_format').
-define(schema_unsupported,          'schema_unsupported').
-define(wrong_required_array,        'wrong_required_array').
-define(not_multiple_of,             'not_multiple_of').
-define(wrong_multiple_of,           'wrong_multiple_of').
-define(too_many_properties,         'too_many_properties').
-define(wrong_max_properties,        'wrong_max_properties').
-define(too_few_properties,          'too_few_properties').
-define(wrong_min_properties,        'wrong_min_properties').
-define(wrong_type_specification,    'wrong_type_specification').
-define(all_schemas_not_valid,       'all_schemas_not_valid').
-define(wrong_all_of_schema_array,   'wrong_all_of_schema_array').
-define(any_schemas_not_valid,       'any_schemas_not_valid').
-define(wrong_any_of_schema_array,   'wrong_any_of_schema_array').
-define(not_one_schema_valid,        'not_one_schema_valid').
-define(wrong_one_of_schema_array,   'wrong_one_of_schema_array').
-define(not_schema_valid,            'not_schema_valid').
-define(wrong_not_schema,            'wrong_not_schema').
-define(invalid_schema,              'wrong_schema').

%%
-define(not_found, not_found).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
