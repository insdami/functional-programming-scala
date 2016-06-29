package l.insdami.funcprogscala.c4

import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec {

  "Using map transform Option[Int] into Option[String]" should "retrieve Option[String]" in {
    assert(Some(1).map(_.toString) == Some("1"))
  }

  "Using map transform None into Option[String]" should "retrieve None" in {
    assert(None.map(_.toString) == None)
  }

  "Perform a valid division and retrieve result as a String" should "retrieve Some(number)" in {
    def div(n1: Int, n2: Int): Option[Int] = if(n2 > 0) Some(n1/n2) else None
    def stringifyDiv(n: Int): Option[String] = Some(s"$n")

    assert(div(1,1).flatMap(stringifyDiv) == Some("1"))
  }

  "Perform a invalid division and retrieve result as a String" should "retrieve None" in {
    def div(n1: Int, n2: Int): Option[Int] = if(n2 > 0) Some(n1/n2) else None
    def stringifyDiv(n: Int): Option[String] = Some(s"$n")

    assert(div(1,0).flatMap(stringifyDiv) == None)
  }

  "getOrElse(0) on Some(1)" should "retrieve 1" in {
    assert(Some(1).getOrElse(0) == 1)
  }

  "getOrElse(0) on None" should "retrieve 0" in {
    assert(None.getOrElse(0) == 0)
  }

  "orElse(Some(0)) on Some(1)" should "retrieve Some(1)" in {
    assert(Some(1).orElse(Some(0)) == Some(1))
  }

  "orElse(Some(0) on None" should "retrieve Some(0)" in {
    assert(None.orElse(Some(0)) == Some(0))
  }

  "If the number is even do not filter it" should "retrieve Some(evennumber)" in {
    assert(Some(2).filter(_ % 2 == 0) == Some(2))
  }

  "If the number is odd filter it" should "retrieve None" in {
    assert(Some(1).filter(_ % 2 == 0) == None)
  }

  "Use a legacy non option function with options ref 4.3" should "use option without modifying the function" in {
    import Option._

    def insuranceRateQuote(age: Int, tickets: Int) = age + tickets * 0.02 //It's not important, just a formula

    def parseInsuranceRateQuote(age: String, noOfSpeddingTickets: String): Option[Double] = {
      val optAge = Try(age.toInt)
      val optTickets = Try(noOfSpeddingTickets.toInt)
      map2(optAge, optTickets)(insuranceRateQuote)
    }

    assert(parseInsuranceRateQuote("28", "5") == Some(insuranceRateQuote(28, 5)))
    assert(parseInsuranceRateQuote("28", "hello") == None)
    assert(parseInsuranceRateQuote("hello", "5") == None)
  }

  "Collect all values from Options" should "retrieve Option list with all values" in {
    import Option._
    val values = (1 to 10).toList
    val optionValues: List[Option[Int]] = values.map(Some(_))
    assert(sequence2(optionValues) == Some(values))
  }

  "Collect all values from Options which contains a None in the middle" should "retrieve a None" in {
    import Option._
    val values = ((1 to 5).toList.map(Some(_)) :+ None) ::: (7 to 10).toList.map(Some(_))
    assert(sequence(values) == None)
  }

}
