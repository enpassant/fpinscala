import fpinscala.errorhandling._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class EitherSpec extends FlatSpec with Matchers with PropertyChecks {

  "Either's map" should "have produce Left when invoked with Left" in {
    val either: Either[String, String] = Left("test")
    assert(either.map(_.toUpperCase) === either)
  }

  it should "have produce Right(TEST) when invoked with Right(test)" in {
    val either: Either[String, String] = Right("test")
    assert(either.map(_.toUpperCase) === Right("TEST"))
  }

  "Either's flatMap" should "have produce Left when invoked with Left" in {
    val either: Either[String, String] = Left("test")
    assert(either.flatMap(x => Right(x.toUpperCase)) === either)
  }

  it should "have produce Left(TEST) when invoked with Right(test)" in {
    val either: Either[String, String] = Right("test")
    assert(either.flatMap(x => Left(x.toUpperCase)) === Left("TEST"))
  }

  it should "have produce Right(TEST) when invoked with Right(test)" in {
    val either: Either[String, String] = Right("test")
    assert(either.flatMap(x => Right(x.toUpperCase)) === Right("TEST"))
  }

  "Either's orElse" should "have produce Right when invoked with Left" in {
    val either: Either[String, String] = Left("test")
    assert(either.orElse(Right("test")) === Right("test"))
  }

  it should "have produce Right(test) when invoked with Right(test)" in {
    val either: Either[String, String] = Right("test")
    assert(either.orElse(Right("TEST")) === Right("test"))
  }

  "Either's map2" should "have produce Left when invoked on Left" in {
    val either: Either[String, String] = Left("test")
    assert(either.map2(Right("test"))(_ + _) === either)
  }

  it should "have produce Left(TEST) when invoked with Left(TEST)" in {
    val either: Either[String, String] = Right("test")
    assert(either.map2(Left("TEST"))(_ + _) === Left("TEST"))
  }

  it should "have produce Right(testTEST) when invoked with Right(TEST)" in {
    val either: Either[String, String] = Right("test")
    assert(either.map2(Right("TEST"))(_ + _) === Right("testTEST"))
  }

  "Either's traverse" should "have produce Right(List) when invoked on Left" in {
    val list: List[String] = List.empty
    assert(Either.traverse(list)(i => Either.Try(i.toInt)) === Right(List.empty))
  }

  it should "have produce Left when invoked with List(1,s)" in {
    val list = List("1", "s")
    val result = Either.traverse(list)(i => Either.Try(i.toInt))
    assert(result match {
      case Left(_) => true
      case _ => false
    })
  }

  it should "have produce Right(List(1,5)) when invoked with List(1,5)" in {
    val list = List("1", "5")
    assert(Either.traverse(list)(i => Either.Try(i.toInt)) === Right(List(1,5)))
  }

  "Either's sequence" should "have produce Some(List()) when invoked on empty list" in {
    val list = List.empty
    assert(Either.sequence(list) === Right(List.empty))
  }

  it should "have produce Left when invoked with List(Right(1),Left(2),Right(5))" in {
    val list = List(Right(1),Left(2),Right(5))
    assert(Either.sequence(list) === Left(2))
  }

  it should "have produce Right(List(1,5)) when invoked with List(Right(1),Right(5))" in {
    val list = List(Right(1),Right(5))
    assert(Either.sequence(list) === Right(List(1,5)))
  }
}
