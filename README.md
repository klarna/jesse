=====
jesse
=====

jesse (JSon Schema Erlang) is an implementation of a json schema validator
for Erlang.

jesse implements [Draft 03] (http://tools.ietf.org/html/draft-zyp-json-schema-03) of
the specification. It supports almost all core schema definitions except:

* format
* $ref

Quick start
-----------

There are two ways of using jesse:

* to use jesse internal in-memory storage to keep all your schema definitions
  In this case jesse will look up a schema definition in its own storage,
  and then validate given json.
* it is also possible to provide jesse with schema definitions when jesse is called.

Examples
--------

    NOTE: jesse doesn't have any parsing functionality, it works with either mochijson2
          format or EEP18 format, so json needs to be parsed in advance, or you can
          specify a callback which jesse will use to parse json.

          In examples below and in jesse test suite jiffy parser is used.

* Use jesse's internal in-memory storage:

(parse json in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"items\": {\"type\": \"integer\"}}">>).
{[{<<"items">>,{[{<<"type">>,<<"integer">>}]}}]}
2> jesse:add_schema(some_key, Schema).
ok
3> Json1 = jiffy:decode(<<"[1, 2, 3]">>).
[1,2,3]
4> jesse:validate(some_key, Json1).
{ok,[1,2,3]}
5> Json2 = jiffy:decode(<<"[1, \"x\"]">>).
[1,<<"x">>]
6> jesse:validate(some_key, Json2).
{error,{data_invalid,<<"x">>,not_integer,
                     {[{<<"type">>,<<"integer">>}]}}}"]")
```

(using a callback)

```erlang
1> jesse:add_schema(some_key,
1>                  <<"{\"uniqueItems\": true}">>,
1>                  fun jiffy:decode/1).
ok
2> jesse:validate(some_key,
2>                <<"[1, 2]">>,
2>                fun jiffy:decode/1).
{ok,[1,2]}
3> jesse:validate(some_key,
3>                <<"[{\"foo\": \"bar\"}, {\"foo\": \"bar\"}] ">>,
3>                fun jiffy:decode/1).
{error,{data_invalid,[{[{<<"foo">>,<<"bar">>}]},
                      {[{<<"foo">>,<<"bar">>}]}],
                     {{[{<<"foo">>,<<"bar">>}]},not_unique},
                     {uniqueItems,true}}}
```

* Call jesse with schema definition in place (do not use internal storage)

(parse json in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"pattern\": \"^a*$\"}">>).
{[{<<"pattern">>,<<"^a*$">>}]}
2> Json1 = jiffy:decode(<<"\"aaa\"">>).
<<"aaa">>
3> jesse:validate_with_schema(Schema, Json1).
{ok,<<"aaa">>}
4> Json2 = jiffy:decode(<<"\"abc\"">>).
<<"abc">>
5> jesse:validate_with_schema(Schema, Json2).
{error,{data_invalid,<<"abc">>,no_match,<<"^a*$">>}}
```

(using a callback)

```erlang
1> Schema = <<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>.
<<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>
2> jesse:validate_with_schema(Schema,
2>                            <<"{\"foo\": 1, \"foooooo\" : 2}">>,
2>                            fun jiffy:decode/1).
{ok,{[{<<"foo">>,1},{<<"foooooo">>,2}]}}
3> jesse:validate_with_schema(Schema,
3>                            <<"{\"foo\": \"bar\", \"fooooo\": 2}">>,
3>                            fun jiffy:decode/1).
{error,{data_invalid,<<"bar">>,not_integer,
                     {[{<<"type">>,<<"integer">>}]}}}""}]""}")
```

Caveats
-------
* pattern and patternProperty attributes:

  jesse uses standard erlang module `re` for regexp matching, therefore there could be
  some incompatible regular expressions in schemas you define.

  From erlang docs: "re's matching algorithms are currently based on the PCRE library,
  but not all of the PCRE library is interfaced"

  But most of common cases should work fine.

Contributing
------------

If you see something missing or incorrect, a pull request is most welcome!