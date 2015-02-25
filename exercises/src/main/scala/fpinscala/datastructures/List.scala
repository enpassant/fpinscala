package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(h, tail)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n <= 0 => l
    case Cons(_, tail) => drop(tail, n -1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) if f(h) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, tail) => 1 + length(tail)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, tail) => Cons(f(h), map(tail)(f))
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A]) {
      (list, item) => Cons(item, list)
    }
  }

  def concatList[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A]) {
      (list, currList) =>
        foldRight(list, currList) {
          (item, cL) => Cons(item, cL)
        }
    }
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) if f(h) => Cons(h, filter(tail)(f))
    case Cons(_, tail) => filter(tail)(f)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, tail) =>
      foldRight(f(h), flatMap(tail)(f)) {
        (item, cL) => Cons(item, cL)
      }
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    List.flatMap(l)( item => if (f(item)) List(item) else Nil )
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def loop[A](l: List[A], sub: List[A], curr: List[A]): Boolean = curr match {
      case Nil => true
      case Cons(hSub, tSub) => l match {
        case Cons(h, t) if h == hSub => loop(t, sub, tSub)
        case Cons(h, t) => loop(t, sub, sub)
        case Nil => false
      }
    }
    loop(l, sub, sub)
  }
}
