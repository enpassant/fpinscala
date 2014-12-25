import fpinscala.datastructures._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class ListSpec extends FlatSpec with Matchers with PropertyChecks {

  "A List's tail" should "have produce Nil when invoked with Nil" in {
    assert(List.tail(Nil) === Nil)
  }

  it should "have produce List(2,3,4) when invoked with List(1,2,3,4)" in {
    assert(List.tail(List(1,2,3,4)) === List(2,3,4))
  }

  it should "have produce the same list as drop(1) for generated integer list" in {
    val intList = Gen.listOf(Gen.choose(0, 100))
    forAll(intList) {list =>
      whenever(true) {
        val l = list.reverse.foldLeft(Nil: List[Int])((l, i) => Cons(i, l))
        val result = List.drop(l, 1)
        List.tail(l) should be === result
      }
    }
  }

  "A List's drop" should "have produce Nil when invoked with Nil" in {
    assert(List.drop(Nil, 1) === Nil)
  }

  it should "have produce List(2,3,4) when invoked with List(1,2,3,4)" in {
    assert(List.drop(List(1,2,3,4), 1) === List(2,3,4))
  }

  it should "have produce Nil for generated integer list when dropped all items" in {
    val intList = Gen.listOf(Gen.choose(0, 100))
    forAll(intList) {list =>
      whenever(true) {
        val l = list.reverse.foldLeft(Nil: List[Int])((l, i) => Cons(i, l))
        val result = List.drop(l, List.length(l))
        result should be (Nil)
      }
    }
  }

  "A List's dropWhile" should "have produce List(2,8,1) when invoked with (List(3,7,2,8,1))(_ > 2)" in {
    def isBig(v: Int) = v > 2
    List.dropWhile(List(3,7,2,8,1), isBig) should be === List(2,8,1)
  }

  "A List's init" should "have produce List(3,7,2,8) when invoked with List(3,7,2,8,1)" in {
    def isBig(v: Int) = v > 2
    List.init(List(3,7,2,8,1)) should be === List(3,7,2,8)
  }

  "A List's length" should "have produce 5 when invoked with (List(3,7,2,8,1))(_ > 2)" in {
    List.length(List(3,7,2,8,1)) should be (5)
  }

  "A List's foldLeft" should "have produce 336 when invoked with (List(3,7,2,8,1))(_ * _)" in {
    List.foldLeft(List(3,7,2,8,1), 1)(_ * _) should be (336)
  }

  "A List's map" should "have produce List(9,21,6,24,3) when invoked with (List(3,7,2,8,1))(_ * 3)" in {
    List.map(List(3,7,2,8,1))(_ * 3) should be === List(9,21,6,24,3)
  }
}
