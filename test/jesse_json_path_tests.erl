%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc EUnit tests for jesse_json_path.
-module(jesse_json_path_tests).
-include_lib("eunit/include/eunit.hrl").

path_aggregate_test() ->
    ?assertEqual(
       [taco, taco, grande],
       jesse_json_path:path(foo, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       3,
       jesse_json_path:path(<<"foo.@count">>, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       6,
       jesse_json_path:path('foo.@sum', [{foo, [1, 2, 3]}])),
    ?assertEqual(
       2.0,
       jesse_json_path:path("foo.@avg", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       1,
       jesse_json_path:path([foo, '@min'], [{foo, [1, 2, 3]}])),
    ?assertEqual(
       3,
       jesse_json_path:path("foo.@max", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       [taco, taco, grande],
       jesse_json_path:path(<<"foo.@unionOfObjects">>,
                            [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       [taco, taco, grande],
       jesse_json_path:path(<<"foo.@unionOfArrays">>,
                            [{foo, [[taco], [taco], [grande]]}])),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(jesse_json_path:path(<<"foo.@distinctUnionOfObjects">>,
                                       [{foo, [taco, taco, grande]}]))),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(jesse_json_path:path(<<"foo.@distinctUnionOfArrays">>,
                                       [{foo, [[taco], [taco], [grande]]}]))),
    ok.

value_aggregate_test() ->
    ?assertEqual(
       6,
       jesse_json_path:value('@sum', [1, 2, 3], [])),
    ?assertEqual(
       6,
       jesse_json_path:value("@sum", [1, 2, 3], [])),
    ?assertEqual(
       6,
       jesse_json_path:value(<<"@sum">>, [1, 2, 3], [])),
    ?assertEqual(
       2.0,
       jesse_json_path:value(<<"@avg">>, [1, 2, 3], [])),
    ?assertEqual(
       [],
       jesse_json_path:value(<<"@avg">>, [], [])),
    ok.

path_edge_test() ->
    ?assertEqual(
       [bar],
       jesse_json_path:path(foo, [[{foo, bar}], [{bar, baz}]])),
    ?assertEqual(
       [bar],
       jesse_json_path:path(foo, [[{foo, bar}], [{bar, baz}]])),
    ok.

value_edge_test() ->
    ?assertEqual(
       [],
       jesse_json_path:value(foo, [{1, 2}], [])),
    ?assertEqual(
       [],
       jesse_json_path:value(<<255>>, [{foo, ok}], [])),
    ?assertEqual(
       [],
       jesse_json_path:value([256], [{foo, ok}], [])),
    ok.

path_plist_test() ->
    lists:foreach(
      fun (F) ->
              ?assertEqual(
                 baz,
                 jesse_json_path:path('foo.bar', F([{foo, [{bar, baz}]}]))),
              ?assertEqual(
                 [],
                 jesse_json_path:path('foo.bar', F([{foo, [{baz, baz}]}]))),
              ?assertEqual(
                 [],
                 jesse_json_path:path('foo.bar', F([{not_foo, ok}]))),
              ?assertEqual(
                 [],
                 jesse_json_path:path('foo.bar', F([])))
      end,
      [fun gb_trees:from_orddict/1, fun dict:from_list/1]),
    ?assertEqual(
       wibble,
       jesse_json_path:path('foo.bar.baz', [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       jesse_json_path:path('foo.bar.baz.invalid_proplist',
                            [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       jesse_json_path:path('foo.bar.baz', [{foo, [{bar, [{bar, wibble}]}]}])),
    ?assertEqual(
       <<"wibble">>,
       jesse_json_path:path('foo.bar.baz',
                            {struct,
                             [{<<"foo">>,
                               {struct,
                                [{<<"bar">>,
                                  {struct, [{<<"baz">>, <<"wibble">>}]}}]}}]})),
    ?assertEqual(
       <<"wibble">>,
       jesse_json_path:path('foo.bar.baz',
                            {[{<<"foo">>,
                               {[{<<"bar">>,
                                  {[{<<"baz">>, <<"wibble">>}]}}]}}]})),
    ?assertEqual(
       "wibble",
       jesse_json_path:path('foo.bar.baz',
                            {struct,
                             [{"foo",
                               {struct,
                                [{"bar",
                                  {struct, [{"baz", "wibble"}]}}]}}]})),
    ?assertEqual(
       ok,
       jesse_json_path:value("foo", [{foo, ok}], [])),
    ?assertEqual(
       ok,
       jesse_json_path:value("foo", [{<<"foo">>, ok}], [])),
    ?assertEqual(
       ok,
       jesse_json_path:value("foo", {}, ok)),
    ok.

to_proplist_readme_test() ->
    ?assertEqual(
       [{<<"foo">>, [{<<"bar">>, <<"baz">>}]}],
       jesse_json_path:to_proplist({struct,
                                    [{<<"foo">>,
                                      {struct,
                                       [{<<"bar">>, <<"baz">>}]}}]})).
