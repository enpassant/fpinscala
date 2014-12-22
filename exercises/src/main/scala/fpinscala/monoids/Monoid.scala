package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = sys.error("todo")

  val intMultiplication: Monoid[Int] = sys.error("todo")

  val booleanOr: Monoid[Boolean] = sys.error("todo")

  val booleanAnd: Monoid[Boolean] = sys.error("todo")

  def optionMonoid[A]: Monoid[Option[A]] = sys.error("todo")

  def endoMonoid[A]: Monoid[A => A] = sys.error("todo")

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    sys.error("todo")

  def count(s: String): Int = 
    sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object WC {
  def apply() = Stub("")

  def apply(str: String) = {
    val indexOf = str.indexOf(' ')
    if (indexOf < 0) Stub(str)
    else {
      val (str1, str2) = str.splitAt(indexOf)
      val lastIndexOf = str2.lastIndexOf(' ') + 1
      val (str3, str4) = str2.splitAt(lastIndexOf)
      Part(str1, str3.count(_ == ' ') - 1, str4)
    }
  }

  def count(wc: WC) = wc match {
    case Stub("") => 0
    case Stub(_) => 1
    case Part("", c, "") => c
    case Part("", c, _) => c + 1
    case Part(_, c, "") => c + 1
    case Part(_, c, _) => c + 2
  }
}

object MonoidTest {

  val wcMonoid = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = a1 match {
      case Stub(s1) => a2 match {
        case Stub(s2) => Stub(s1 + s2)
        case Part(ls1, w, rs1) => Part(s1 + ls1, w, rs1)
      }
      case Part(ls1, w1, rs1) => a2 match {
        case Stub(s2) => Part(ls1, w1, rs1 + s2)
        case Part(ls2, w2, rs2) =>
          Part(ls1,
            w1 + w2 + (if (rs1 == "" && ls2 == "") 0 else 1),
            rs2)
      }
    }

    def zero: WC = WC()
  }

  def counter(str: String): WC = {
    val length = str.length
    if (length < 10) {
      WC(str)
    } else {
      val (str1, str2) = str.splitAt(length / 2)
      wcMonoid.op(counter(str1), counter(str2))
    }
  }

  def main(args: Array[String]) {
    val longText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

    val splitLength = 5
    val slideList = longText.sliding(splitLength, splitLength).toList
    val splits: List[WC] = slideList.map( text => WC(text) )

    println(longText)
    val result = splits.foldLeft(wcMonoid.zero)(wcMonoid.op)
    println(result)
    val result2 = splits.foldRight(wcMonoid.zero)(wcMonoid.op)
    println(result2)
    val result3 = counter(longText)
    println(result3)
    println(WC.count(result))
  }
}

