import fpinscala.state._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class StateSpec extends FlatSpec with Matchers with PropertyChecks {

  "RNG's nonNegativeInt" should "have produce a nonNegativeInt and a new RNG" in {
    def generateNrandomNumber(n: Int, rng: RNG): (Int, RNG) = {
      val value = RNG.nonNegativeInt(rng)
      assert(value._1 >= 0 && value._1 <= Int.MaxValue)
      if (n > 0) generateNrandomNumber(n - 1, value._2)
      else value
    }
    generateNrandomNumber(10000, RNG.Simple(0))
  }

  "RNG's double" should "have produce a Double between 0 and 1, not including 1" in {
    def generateNrandomNumber(n: Int, rng: RNG): (Double, RNG) = {
      val value = RNG.double(rng)
      assert(value._1 >= 0.0 && value._1 < 1.0)
      if (n > 0) generateNrandomNumber(n - 1, value._2)
      else value
    }
    generateNrandomNumber(10000, RNG.Simple(0))
  }

  "RNG's doubleNew" should "have produce a Double between 0 and 1, not including 1" in {
    def generateNrandomNumber(n: Int, rng: RNG): (Double, RNG) = {
      val value = RNG.doubleNew(rng)
      assert(value._1 >= 0.0 && value._1 < 1.0)
      if (n > 0) generateNrandomNumber(n - 1, value._2)
      else value
    }
    generateNrandomNumber(10000, RNG.Simple(0))
  }

}
