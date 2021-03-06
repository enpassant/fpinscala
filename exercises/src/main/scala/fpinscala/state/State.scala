package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (v, nextRng) if v == Int.MinValue => (0, nextRng)
    case (v, nextRng) => (v.abs, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val value = nonNegativeInt(rng)
    (value._1.toDouble / (1.0 + Int.MaxValue), value._2)
  }

  def doubleNew(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_.toDouble / (1.0 + Int.MaxValue))(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val v1 = nonNegativeInt(rng)
    val v2 = double(v1._2)
    ((v1._1, v2._1), v2._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val v1 = double(rng)
    val v2 = nonNegativeInt(v1._2)
    ((v1._1, v2._1), v2._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val v1 = double(rng)
    val v2 = double(v1._2)
    val v3 = double(v2._2)
    ((v1._1, v2._1, v3._1), v3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft((Nil: List[Int], rng)) {
      (v, index) =>
        val (i, rng2) = v._2.nextInt
        (v._1 ++ List(i), rng2)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
