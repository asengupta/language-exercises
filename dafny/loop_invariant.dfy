method Count(n:int) returns (i:int)
requires n >= 0
{
    i := 0;

    while i < n
        invariant 0 <= i <= n
    {
        i := i + 1;
    }

    assert i == n;
}

method CountDown(n:int) returns (i:int)
    requires n >= 0
{
    i := n;

    while i > 0
        invariant 0 <= i <= n
    {
        i := i - 1;
    }

    assert i == 0;
}