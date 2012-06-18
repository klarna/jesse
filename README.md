jesse
=====

jesse (JSon Schema Erlang) is an implementation of a json schema validator
for Erlang. Currently, it implements most of "useful" parts of the following
standard draft: http://tools.ietf.org/html/draft-zyp-json-schema-03.

A list of non-implemented features could be found in jesse_schema_validator.erl.
As soon as there are new drafts coming, the implementation will be updated
accordingly.

Quick start
-----------

* Add jesse as a rebar dependency to your application.
* Update schema definitions:

```erlang
1> jesse:update_schema("path/to/json/schema/files/", fun jiffy:decode/1)
```

* Use the validation function whenever you need:

```erlang
2> jesse:validate("message-v1.json#", jiffy:decode(<<"{\"text\": \"some text\"}">>)).
{ok,{[{<<"text">>,<<"some text">>}]}}
3> jesse:validate("message-v1.json#", jiffy:decode(<<"{\"text\": 42}">>)).
{error,{data_invalid,{<<"text">>,42},
                     not_string,
                     {[{<<"title">>,<<"Message text">>},
                       {<<"type">>,<<"string">>},
                       {<<"example">>,<<"hello, world">>}]}}}
```

More documentation is coming soon...