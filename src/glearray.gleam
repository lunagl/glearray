pub type Array(a)

@external(erlang, "glearray_ffi", "new")
@external(javascript, "./glearray_ffi.mjs", "new_array")
pub fn new() -> Array(a)

@external(erlang, "erlang", "tuple_size")
@external(javascript, "./glearray_ffi.mjs", "array_length")
pub fn length(array: Array(a)) -> Int
