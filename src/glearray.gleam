pub type Array(a)

@external(erlang, "glearray", "new")
@external(javascript, "./glearray.mjs", "new_array")
pub fn new() -> Array(a)

@external(erlang, "erlang", "tuple_size")
@external(javascript, "./glearray.mjs", "array_length")
pub fn length(array: Array(a)) -> Int
