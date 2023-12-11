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
  case index >= 0 && index < length(array) {
    True -> Ok(do_at(array, index))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "at")
@external(javascript, "./glearray_ffi.mjs", "at")
fn do_at(array: Array(a), index: Int) -> a
