export function newArray() {
  return [];
}

export function fromList(list) {
  return list.toArray();
}

export function arrayLength(array) {
  return array.length;
}

export function at(array, index) {
  return array[index];
}

export function set(array, index, value) {
  const copy = [...array];
  copy[index] = value;
  return copy;
}
