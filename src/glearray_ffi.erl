-module(glearray_ffi).

-export([new/0, get/2, set/3, insert/3]).

new() -> {}.

get(Array, Index) ->
  case catch element(Index + 1, Array) of
    {'EXIT', _} -> {error,nil};
    E -> {ok,E}
  end.

set(Array, Index, Value) -> setelement(Index + 1, Array, Value).

insert(Array, Index, Value) -> erlang:insert_element(Index + 1, Array, Value).
