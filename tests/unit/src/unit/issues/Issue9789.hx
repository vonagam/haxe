package unit.issues;
import haxe.ds.Option;
import haxe.ds.Either;

class Issue9789 extends Test {
  function test() {
    eq(1,switchFallback(Some(1),2));
    eq(2,switchFallback(None,2));

    eq(11,switchTuples(Some(1),Some(1)));
    eq(10,switchTuples(Some(2),Some(3)));
    eq(12,switchTuples(None,Some(3)));
    eq(-1,switchTuples(Some(33),Some(1)));
    eq(-9,switchTuples(Some(-9),Some(1)));
    eq(-1,switchTuples(None,Some(1)));

    eq(14,swithFoo(Two(12,2)));
    eq(54,swithFoo(One(55)));
    eq(18,swithFoo(Zero));

    eq(0, switchEither(Left(1)));
    eq(-10, switchEither(Left(-1)));
    eq(16, switchEither(Left(4)));
    eq(10, switchEither(Right(1)));
    eq(0, switchEither(Right(-1)));
    eq(0, switchEither(Right(-4)));
    eq(25, switchEither(Right(-5)));
  }

  function switchFallback(option: Option<Int>, fallback: Int) {
    return switch (option) {
      case
        Some(int) |
        None.set(int=fallback)
      :
        int;
    };
  }

  function switchTuples(optionA: Option<Int>, optionB: Option<Int>) {
    return switch [optionA, optionB] {
      case
        [Some(1),Some(1)].set(key=11) |
        [Some(2.set(final key=10)),_] |
        [_,(_ => Some(3)).set(var key=12)] |
        [Some(key).where(key < 0),_] |
        [_,_].set(key=-1)
      :
        key;
    };
  }

  function swithFoo(foo: Foo) {
    return switch (foo) {
      case
        Two(x,y) |
        One(x).set(y=-1) |
        Zero.set(x=9,y=9)
      :
        x + y;
    }
  }

  function switchEither(either: Either<Int, Int>) {
    return switch (either) {
      case
        Left(int).where(int < 0) |
        Right(int).where(int > 0)
      :
        int * 10;
      case
        Left(_ * _ => int.where(int > 9)) |
        Right(_ * _ => int.where(int > 16)):
        int;
      case _: 0;
    }
  }
}

private enum Foo {
  Zero;
  One(x: Int);
  Two(x: Int, y: Int);
}
