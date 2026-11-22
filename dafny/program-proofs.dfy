ghost method Triple(x: int) returns (r: int) {
  var y := x * 2;
  r := x + y;
  assert r == x * 3;
}


method TripleEven(x: int)
returns (r: int)
requires x % 2 == 0
{
  var y := x / 2;
  r := y * 6;
  assert r == x * 3;
}

method Min(x: int, y: int) returns (m: int)
ensures m <= x && m <= y
{
  if {
    case (x >= 0) && (y >= 0) =>
      m := - x - y - 1;
    case (x >= 0) && (y <= 0) =>
      m := -x + y - 1;
    case (x <= 0) && (y >= 0) =>
      m := x - y - 1;
    case (x <= 0) && (y <= 0) =>
      m := x + y -1;
  }
}

method MaxSum(x: int, y: int) returns (sum: int, max: int)
ensures (max >= x) && (max >= y)
ensures (max == x) || (max == y)
ensures (sum == x + y)
{
  max := if (x >= y) then x else y;
  sum := x + y;
}

method CallMaxSum() {
  var sum, max := MaxSum(1928, 1);
  assert sum == 1929;
  assert max == 1928;
}

method ReconstructFromMaxSum(sum: int, max: int) returns (x: int, y: int)
ensures sum == x + y
ensures (max >= x && max >= y) && (max == x || max == y)
requires max >= sum - max
{
  x := max;
  y := sum - x;
}

method TestMaxSum(x: int, y: int) {
  var s, m := MaxSum(x, y);
  var xx, yy := ReconstructFromMaxSum(s, m);
  assert (xx == x && yy == y) || (xx == y && yy == x);
}

function Average(x: int, y: int) : int
{
  (x + y) / 2
}

method Triple'(x: int) returns (r: int)
ensures Average(r, 3 * x) == 3 * x {
  r := 3 * x + 1;
}

method Triple''(x: int) returns (r: int)
ensures Average(2 * r, 6 * x) == 6 * x
{
  r := 3 * x;
}

function F(): int {
  29
}

method M() returns (r: int) ensures (r == 29){
  r := 29;
}

method Caller() {
  var a := F();
  var b := M();
  assert a == 29;
  assert b == 29;
}
