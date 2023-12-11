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
  let list = ["a", "B", "fdsau", "rh3892wfd", "äèëåäº£", "Another ONE!"]
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
