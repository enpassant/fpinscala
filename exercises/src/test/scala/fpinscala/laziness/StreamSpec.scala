import fpinscala.laziness._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class StreamSpec extends FlatSpec with Matchers with PropertyChecks {

  val stream = Stream(3, 8, 2, 23, 1, 6, 7, 2, 3, 2, 11)

  "Stream's toList" should "have produce Nil when invoked with empty" in {
    assert(Stream.empty.toList == Nil)
  }

  it should "have produce List(3,8,2,23,1,6,7,2,3,2,11) when invoked with Stream(3,8,2,23,1,6,7,2,3,2,11)" in {
    assert(stream.toList === List(3,8,2,23,1,6,7,2,3,2,11))
  }

  "Stream(3,8,2,23,1,6,7,2,3,2,11)'s take" should "have produce List(3,8,2,23,1) when invoked with take(5)" in {
    assert(stream.take(5).toList === List(3,8,2,23,1))
  }

  "Stream(3,8,2,23,1,6,7,2,3,2,11)'s takeWhile" should "have produce List(3,8,2,23,1,6) when invoked with takeWhile(_ != 7)" in {
    assert(stream.takeWhile(_ != 7).toList === List(3,8,2,23,1,6))
  }

  "Stream(3,8,2,23,1,6,7,2,3,2,11)'s drop" should "have produce List(23,1,6,7,2,3,2,11) when invoked with drop(3)" in {
    assert(stream.drop(3).toList === List(23,1,6,7,2,3,2,11))
  }

  "Stream(3,8,2,23,1,6,7,2,3,2,11)'s forAll" should "have produce true when invoked with forAll(_ < 24)" in {
    assert(stream.forAll(_ < 24))
  }

  it should "have produce false when invoked with forAll(_ < 23)" in {
    assert(!stream.forAll(_ < 23))
  }

  "Stream's from" should "have produce List(3,4,5,6) when invoked with from(3).take(4).toList" in {
    assert(Stream.from(3).take(4).toList === List(3,4,5,6))
  }

  "Stream's unfold" should "have produce List(3,4,5,6) when invoked with unfold(3)(s => Some((n, n + 1))).take(4).toList" in {
    assert(Stream.unfold(3)(n => Some((n, n + 1))).take(4).toList === List(3,4,5,6))
  }

  "Stream's fib" should "have produce List(1,1,2,3,5,8,13,21,34,55) when invoked with fib.take(10).toList" in {
    assert(Stream.fib.take(10).toList === List(1,1,2,3,5,8,13,21,34,55))
  }
}
