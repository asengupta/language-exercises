method AddOne(x:int) returns (y:int)
    ensures y == x + 1
{
    y := x + 1;
}

method Add(a:int, b:int) returns (c:int)
    ensures c == a + b
{
    c := a + b;
}

method Ex6(a:int)
    requires a > 5
{
    var b := a - 2;
    assert b > 3;
}
