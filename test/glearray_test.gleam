import gleam/function
import gleam/list
import gleam/result
import glearray.{type Array}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn empty_array_test() {
  assert_empty(glearray.new())
}

pub fn from_to_list_test() {
  []
  |> glearray.from_list
  |> assert_empty
  |> glearray.to_list
  |> should.equal([])

  let list = [1, 2, 3, -8, 8954]
  list
  |> glearray.from_list
  |> assert_length(5)
  |> glearray.to_list
  |> should.equal(list)
}

pub fn get_test() {
  let list = ["a", "B", "fdsau", "rh3892wfd", "äèëåäº£"]
  let array = glearray.from_list(list)
  list.index_map(list, fn(element, index) {
    array
    |> glearray.get(index)
    |> should.equal(Ok(element))
  })

  glearray.get(array, -1)
  |> should.be_error
  glearray.get(array, glearray.length(array))
  |> should.be_error
  glearray.get(array, 100)
  |> should.be_error
  glearray.get(glearray.new(), 0)
  |> should.be_error
}

pub fn get_or_default_test() {
  let list = [5, 4, 3, 2, 1]
  let array = glearray.from_list(list)
  list.index_map(list, fn(element, index) {
    array
    |> glearray.get_or_default(index, -1)
    |> should.equal(element)
  })

  glearray.get_or_default(array, -1, -1)
  |> should.equal(-1)
  glearray.get_or_default(array, glearray.length(array), 42)
  |> should.equal(42)
  glearray.get_or_default(array, 100, -1)
  |> should.equal(-1)
  glearray.get_or_default(glearray.new(), 0, 0)
  |> should.equal(0)
}

pub fn set_test() {
  let array = glearray.from_list([1, 2, 3, 4])
  let assert Ok(modified) = glearray.copy_set(in: array, at: 1, value: 10)
  array
  |> glearray.to_list
  |> should.equal([1, 2, 3, 4])
  modified
  |> glearray.to_list
  |> should.equal([1, 10, 3, 4])

  glearray.copy_set(array, -1, 0)
  |> should.be_error
  glearray.copy_set(array, glearray.length(array), 0)
  |> should.be_error
  glearray.copy_set(glearray.new(), 0, 0)
  |> should.be_error
}

pub fn push_test() {
  glearray.new()
  |> glearray.copy_push(1)
  |> glearray.copy_push(2)
  |> glearray.copy_push(3)
  |> glearray.copy_push(4)
  |> glearray.to_list
  |> should.equal([1, 2, 3, 4])

  // Ensure immutability; relevant for the JS impl
  glearray.from_list([1, 2, 3])
  |> function.tap(glearray.copy_push(_, 4))
  |> glearray.to_list
  |> should.equal([1, 2, 3])
}

pub fn insert_test() {
  let array = glearray.from_list([1, 2, 3])
  array
  |> glearray.copy_insert(at: 0, value: 9)
  |> result.map(glearray.to_list)
  |> should.equal(Ok([9, 1, 2, 3]))
  array
  |> glearray.copy_insert(at: 2, value: -1)
  |> result.map(glearray.to_list)
  |> should.equal(Ok([1, 2, -1, 3]))
  array
  |> glearray.copy_insert(3, 20)
  |> result.map(glearray.to_list)
  |> should.equal(Ok([1, 2, 3, 20]))
}

fn assert_empty(array: Array(a)) -> Array(a) {
  assert_length(array, 0)
  should.equal(array, glearray.new())
  array
}

fn assert_length(array: Array(a), length: Int) -> Array(a) {
  array
  |> glearray.length
  |> should.equal(length)
  array
}
