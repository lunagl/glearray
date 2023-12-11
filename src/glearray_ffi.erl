-module(glearray_ffi).

-export([new/0, at/2]).

new() -> {}.

at(Array, Index) -> element(Index + 1, Array).
