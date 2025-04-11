-module(glearray_ffi).

-export([new/0, get/2, get_or_default/3, set/3, insert/3]).

new() -> {}.

get(Array, Index) ->
  case catch element(Index + 1, Array) of
    {'EXIT', _} -> {error,nil};
    E -> {ok,E}
  end.

get_or_default(Array, Index, Default) ->
  case catch element(Index + 1, Array) of
    {'EXIT', _} -> Default;
    E -> E
  end.

set(Array, Index, Value) ->
  case catch setelement(Index + 1, Array, Value) of
    {'EXIT', _} -> {error,nil};
    A -> {ok,A}
  end.

insert(Array, Index, Value) ->
  case catch erlang:insert_element(Index + 1, Array, Value) of
    {'EXIT', _} -> {error,nil};
    A -> {ok,A}
  end.
