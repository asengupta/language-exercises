function Inc(x:int): int
{
    x + 1
}

method TestInc()
{
    assert Inc(5) == 6;
}

function Abs(x:int): int
{
    if x >= 0 then x else -x
}

method TestAbs(x:int)
{
    assert Abs(x) >= 0;
}

function Max(a: int, b: int): int
{
    if a > b then a else b
}

method TestMax(a: int, b: int)
{
    assert Max(a,b) >= a;
    assert Max(a,b) >= b;
}

method Triple(n:int) returns (s:int)
    requires n >= 0
{
    s := 0;
    var i := 0;

    while i < n
        invariant 0 <= i <= n
        invariant s == i * 3
    {
        s := s + 3;
        i := i + 1;
    }

    assert s == 3 * n;
}