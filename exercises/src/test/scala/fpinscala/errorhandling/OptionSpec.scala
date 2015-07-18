import fpinscala.errorhandling._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class OptionSpec extends FlatSpec with Matchers with PropertyChecks {

  "Option's map" should "have produce None when invoked on None" in {
    val optValue: Option[Int] = None
    assert(optValue.map(_ * 2) === None)
  }

  it should "have produce 4 when invoked on Some(2)" in {
    val optValue: Option[Int] = Some(2)
    assert(optValue.map(_ * 2) === Some(4))
  }

  "Option's getOrElse" should "have produce 0 when invoked on None" in {
    val optValue: Option[Int] = None
    assert(optValue.getOrElse(0) === 0)
  }

  it should "have produce 4 when invoked on Some(4)" in {
    val optValue: Option[Int] = Some(4)
    assert(optValue.getOrElse(0) === 4)
  }

  "Option's flatMap" should "have produce None when invoked on None" in {
    val optValue: Option[Int] = None
    assert(optValue.flatMap(x => Some(x * 2)) === None)
  }

  it should "have produce Some(4) when invoked on Some(_ * 2)" in {
    val optValue: Option[Int] = Some(2)
    assert(optValue.flatMap(x => Some(x * 2)) === Some(4))
  }

  "Option's orElse" should "have produce Some(0) when invoked on None" in {
    val optValue: Option[Int] = None
    assert(optValue.orElse(Some(0)) === Some(0))
  }

  it should "have produce Some(4) when invoked on Some(4)" in {
    val optValue: Option[Int] = Some(4)
    assert(optValue.orElse(Some(0)) === Some(4))
  }

  "Option's filter" should "have produce None when invoked on None" in {
    val optValue: Option[Int] = None
    assert(optValue.filter(_ >= 5) === None)
  }

  it should "have produce None when invoked on Some(2)" in {
    val optValue: Option[Int] = Some(2)
    assert(optValue.filter(_ >= 5) === None)
  }

  it should "have produce Some(6) when invoked on Some(6)" in {
    val optValue: Option[Int] = Some(6)
    assert(optValue.filter(_ >= 5) === Some(6))
  }

  "Variance" should "have produce None when invoked on empty seq" in {
    val seq = Seq.empty
    assert(Option.variance(seq) === None)
  }

  it should "have produce 2 when invoked with Seq(1,2,3,4,5)" in {
    val seq = Seq(1.0,2.0,3.0,4.0,5.0)
    assert(Option.variance(seq) === Some(2))
  }

  "Sequence" should "have produce Some(List()) when invoked on empty list" in {
    val list = List.empty
    assert(Option.sequence(list) === Some(List.empty))
  }

  it should "have produce None when invoked with List(Some(1),None,Some(5))" in {
    val list = List(Some(1),None,Some(5))
    assert(Option.sequence(list) === None)
  }

  it should "have produce List(Some(1),Some(5)) when invoked with Some(List(1,5))" in {
    val list = List(Some(1),Some(5))
    assert(Option.sequence(list) === Some(List(1,5)))
  }

  "Map2" should "have produce None when invoked with None" in {
    def fn(a: Int, b: Int) = a + b
    assert(Option.map2(None, None)(fn) === None)
    assert(Option.map2(Some(2), None)(fn) === None)
    assert(Option.map2(None, Some(2))(fn) === None)
  }

  it should "have produce Some(5) when invoked with Some(2), Some(3)" in {
    def fn(a: Int, b: Int) = a + b
    assert(Option.map2(Some(2), Some(3))(fn) === Some(5))
  }
}

