-module(glearray_ffi).

-export([new/0, get/2, get_or_default/3, set/3, insert/3]).

new() -> {}.

get(Array, Index) ->
    try element(Index + 1, Array) of
        E -> {ok, E}
    catch
        error:badarg -> {error, nil}
    end.

get_or_default(Array, Index, Default) ->
    try element(Index + 1, Array) of
        E -> E
    catch
        error:badarg -> Default
    end.

set(Array, Index, Value) ->
    try setelement(Index + 1, Array, Value) of
        A -> {ok, A}
    catch
        error:badarg -> {error, nil}
    end.

insert(Array, Index, Value) ->
    try erlang:insert_element(Index + 1, Array, Value) of
        A -> {ok, A}
    catch
        error:badarg -> {error, nil}
    end.
