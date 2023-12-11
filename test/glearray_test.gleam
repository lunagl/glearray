import gleam/list
import gleeunit
import gleeunit/should
import glearray.{type Array}

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

pub fn at_test() {
  let list = ["a", "B", "fdsau", "rh3892wfd", "äèëåäº£"]
  let array = glearray.from_list(list)
  list.index_map(
    list,
    fn(index, element) {
      array
      |> glearray.at(index)
      |> should.equal(Ok(element))
    },
  )

  glearray.at(array, -1)
  |> should.be_error
  glearray.at(array, glearray.length(array))
  |> should.be_error
  glearray.at(array, 100)
  |> should.be_error
  glearray.at(glearray.new(), 0)
  |> should.be_error
}

pub fn set_test() {
  let array = glearray.from_list([1, 2, 3, 4])
  let assert Ok(modified) = glearray.set(in: array, at: 1, value: 10)
  array
  |> glearray.to_list
  |> should.equal([1, 2, 3, 4])
  modified
  |> glearray.to_list
  |> should.equal([1, 10, 3, 4])

  glearray.set(array, -1, 0)
  |> should.be_error
  glearray.set(array, glearray.length(array), 0)
  |> should.be_error
  glearray.set(glearray.new(), 0, 0)
  |> should.be_error
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
