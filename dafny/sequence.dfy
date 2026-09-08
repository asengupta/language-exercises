method SeqExample()
{
    var s := [10,20,30];

    assert s[0] == 10;
    assert s[1] == 20;
    assert |s| == 3;
}

method SeqExample2()
{
    var s := [10,20,30];

    var t := s[1..];

    assert |t| == 2;
}

method SeqExample3()
{
    var s := [1,2,3];

    var t := s + [4];

    assert |t| == 4;
}

method ArrayExample(a: array<int>)
    requires a.Length >= 3
{
    var s := a[..];

    assert |s| == a.Length;
}

method ArrayExample2(a: array<int>)
    requires a.Length >= 3
    modifies a
{
    var older := a[..];

    a[0] := 10;

    assert a[1..] == older[1..];
}

method Update(a: array<int>)
    requires a.Length > 0
    modifies a
{
    var older := a[..];

    a[0] := a[0] + 1;

    assert a[1..] == older[1..];
}

method FrameTest(a: array<int>, b: array<int>)
    requires a.Length > 0
    requires a != b
    requires b.Length > 0
    modifies a
{
    a[0] := 1;

    assert b[0] == old(b[0]);
}

method FrameTest2(a: array<int>, b: array<int>)
    requires a.Length > 0
    requires b.Length > 0
    modifies a, b
{
    b[0] := 5;
}
