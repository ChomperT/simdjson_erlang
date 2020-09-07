-module(simdjson).
-export([decode/1, lazy_decode/1, at/2, erase/1, erase_all/0]).
-on_load(init/0).

init() ->
        ok = erlang:load_nif("./simdjson_nif", 0). 

decode(_X) ->
        exit(nif_library_not_load).

lazy_decode(_X) ->
        exit(nif_library_not_load).

at(_X, _Y) ->
        exit(nif_library_not_load).

erase(_X) ->
        exit(nif_library_not_load).

erase_all() ->
        exit(nif_library_not_load).

