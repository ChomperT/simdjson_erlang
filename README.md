simdjson
=====

This is an erlang bindings from [simdjson](https://github.com/simdjson/simdjson)

Build
-----

    $ rebar3 compile

-----
Normal Mode
----
    2> simdjson:decode("[1,2,3,4,5]").
    [1,2,3,4,5]

    3> simdjson:decode("{\"a\": 1, \"b\": 2}").
    #{<<"a">> => 1,<<"b">> => 2}
----
Lazy Mode
----
    1>  Id = simdjson:lazy_decode("{\"a\": 1, \"b\": 2, \"c\": {\"a\": 1, \"b\": 2, \"c\": [1,2,3,4,5]}}").
    2129395095
    2> simdjson:at(Id, "a").
    1
    3> simdjson:at(Id, "c").
    #{<<"a">> => 1,<<"b">> => 2,<<"c">> => [1,2,3,4,5]}
    4> simdjson:at(Id, "c/b").
    2
    5> simdjson:at(Id, "c/c").
    [1,2,3,4,5]
    6> simdjson:at(Id, "c/c/0").
    1
    7> simdjson:at(Id, "c/c/4").
    5
    8> simdjson:erase(Id).  %remove this lazy document;
    ok
    9> simdjson:erase_all(). % or remove all lazy document;
    ok
----
