method Simple()
{
    var x := 2;
    var y := x + 3;

    assert y == 5;
}

method Ex1()
{
    var a := 7;
    var b := a * 2;
    var c := b - a;

    assert c == 7;
}

method Ex2(x:int)
{
    var y := x + 1;
    var z := y + 1;

    assert z == x + 2;
}

method Ex3(a:int)
{
    assume a > 5;

    var b := a - 2;

    assert b > 3;
}
