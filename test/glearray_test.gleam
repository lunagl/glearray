import gleeunit
import gleeunit/should
import glearray

pub fn main() {
  gleeunit.main()
}

pub fn empty_array_test() {
  glearray.new()
  |> glearray.length
  |> should.equal(0)
}
