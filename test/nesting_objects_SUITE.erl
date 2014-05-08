-module(nesting_objects_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> 
  [book_country, book_required_missing].

book_required_missing(Config) -> 
  Schema = book_schema(Config),
  Data = book_required_missing_data(Config),
  {error,
   [{data_invalid,
     {[{<<"article">>,
	{[{<<"type">>,<<"object">>},
	  {<<"required">>,true},
	  {<<"properties">>,
	   {[{<<"country">>,
	      {[{<<"required">>,true},
		{<<"enum">>,
		 [<<"EUR">>,<<"USD">>,
		  <<"RUB">>]}]}}]}}]}}]},
     wrong_type,[],
     [<<"books">>,<<"connection">>,<<"patterns">>,0]}]} = jesse:validate_with_schema(Schema, Data).

book_country(Config) -> 
  Schema = book_schema(Config),
  Data = book_data(Config),
  {error,
   [{data_invalid,
     {[{<<"required">>,true},
       {<<"enum">>,[<<"EUR">>,<<"USD">>,<<"RUB">>]}]},
     not_in_range,<<"RUBBER">>,
     [<<"books">>,<<"connection">>,<<"patterns">>,0,
      <<"country">>]}]} = jesse:validate_with_schema(Schema, Data).

book_schema(Config) ->
  Path = ?config(data_dir, Config),
  {ok, F} = file:read_file(Path ++ "/book_schema.json"),
  AA = jiffy:decode(F),
  BB = jiffy:decode(binary_to_list(F)),
  AA = BB,
  jiffy:decode(F).

book_data(Config) ->
  Path = ?config(data_dir, Config),
  {ok, F} = file:read_file(Path ++ "/book_invalid_country.data"),
  jiffy:decode(F).

book_required_missing_data(Config) ->
  Path = ?config(data_dir, Config),
  {ok, F} = file:read_file(Path ++ "/book_country_missing.data"),
  jiffy:decode(F).
