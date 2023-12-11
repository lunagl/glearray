-module(glearray_ffi).

-export([new/0, at/2, set/3]).

new() -> {}.

at(Array, Index) -> element(Index + 1, Array).

set(Array, Index, Value) -> setelement(Index + 1, Array, Value).
