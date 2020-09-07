-module(simdjson).
-export([decode/1, lazy_decode/1, at/2, erase/1, erase_all/0]).
-on_load(init/0).

-define(LIBNAME, simdjson).

init() ->
        SoName = case filelib:is_dir(filename:join(["..", priv])) of
    	        true ->
    		        filename:join(["..", priv, ?LIBNAME]);
                _ ->
                        filename:join([priv, ?LIBNAME])
        end,
        erlang:load_nif(SoName, 0).

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

