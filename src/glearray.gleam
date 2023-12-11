import gleam/iterator.{type Iterator}

pub type Array(a)

@external(erlang, "glearray_ffi", "new")
@external(javascript, "./glearray_ffi.mjs", "newArray")
pub fn new() -> Array(a)

@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "./glearray_ffi.mjs", "fromList")
pub fn from_list(list: List(a)) -> Array(a)

@external(erlang, "erlang", "tuple_to_list")
@external(javascript, "./gleam.mjs", "toList")
pub fn to_list(array: Array(a)) -> List(a)

@external(erlang, "erlang", "tuple_size")
@external(javascript, "./glearray_ffi.mjs", "arrayLength")
pub fn length(of array: Array(a)) -> Int

pub fn at(in array: Array(a), get index: Int) -> Result(a, Nil) {
  case is_valid_index(array, index) {
    True -> Ok(do_at(array, index))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "at")
@external(javascript, "./glearray_ffi.mjs", "at")
fn do_at(array: Array(a), index: Int) -> a

pub fn set(
  in array: Array(a),
  at index: Int,
  value value: a,
) -> Result(Array(a), Nil) {
  case is_valid_index(array, index) {
    True -> Ok(do_set(array, index, value))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "set")
@external(javascript, "./glearray_ffi.mjs", "set")
fn do_set(array: Array(a), index: Int, value: a) -> Array(a)

fn is_valid_index(array: Array(a), index: Int) -> Bool {
  index >= 0 && index < length(array)
}

@external(erlang, "erlang", "append_element")
@external(javascript, "./glearray_ffi.mjs", "push")
pub fn push(onto array: Array(a), value value: a) -> Array(a)

pub fn insert(
  into array: Array(a),
  at index: Int,
  value value: a,
) -> Result(Array(a), Nil) {
  case index >= 0 && index <= length(array) {
    True -> Ok(do_insert(array, index, value))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "insert")
@external(javascript, "./glearray_ffi.mjs", "insert")
fn do_insert(array: Array(a), index: Int, value: a) -> Array(a)

pub fn iterate(array: Array(a)) -> Iterator(a) {
  use index <- iterator.unfold(from: 0)
  case at(array, index) {
    Ok(element) -> iterator.Next(element, index + 1)
    Error(_) -> iterator.Done
  }
}
