package l.insdami.funcprogscala.c4

import org.scalatest.FlatSpec

class EitherSpec extends FlatSpec {

  it should "map the Int value to String" in {
    val either: Either[Exception, Int] = Right(1)
    assert(either.map(_.toString) == Right("1"))
  }

  it should "not map a value" in {
    val either: Either[Exception, Int] = Left(new Exception("I do not like you"))
    assert(either.map(_.toString) == either)
  }

  it should "flatMap the Int value to String" in {
    val either = Right(1)
    assert(either.flatMap(v => Right(v.toString)) == Right("1"))
  }

  it should "not flatMap a value" in {
    val either = Left(new Exception("I do not like you"))
    assert(either.map(_.toString) == either)
  }

  it should "retrieve the original either" in {
    val either = Right("1")
    assert(either.orElse(Right("2")) == either)
  }

  it should "retrieve the default value" in {
    val either = Left("nope")
    assert(either.orElse(Right("2")) == Right("2"))
  }

  it should "retrieve the division" in {
    val one = Right(1)
    val division = one.map2_forcomp(_: Either[String, Int])(_ / _)
    assert(division(one) == one)
  }

  it should "retrieve left side" in {
    val one = Right(1)
    val division = one.map2(_: Either[Exception, Int])(_ / _)
    val arithmeticException = new ArithmeticException("/ by zero")
    assert(division(Left(arithmeticException)) == Left(arithmeticException))
  }

  it should "sequence retrieving left" in {
    assert(Either.sequence(List(Right(1), Left("nope"), Right(2))) == Left("nope"))
  }

  it should "sequence retrieving a right of list ints" in {
    assert(Either.sequence(List(Right(1), Right(2), Right(3))) == Right(List(1,2,3)))
  }
}
