package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, ls) => ls
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case n if (n <= 0) => l
    case _ => drop(tail(l), n - 1)
  }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => 1 + y)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumByFoldLeft(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def productByFoldLeft(l: List[Int]) = {
    foldLeft(l, 1)(_ * _)
  }

  def lenghtByFoldLeft(l: List[Int]) = {
    foldLeft(l, 0)((x, _) => x + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft[A, List[A]](l, Nil)((h, t) => Cons(t, h))
  }

  def foldrightbyfoldleft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft[A, B](reverse(l), z)((b, a) => f(a, b))
  }

  def appendbyfold[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def appendbyfold_1[A](l1: List[A], l2: List[A]): List[A] = {
    foldrightbyfoldleft[A, List[A]](l1, l2)(Cons(_, _))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldrightbyfoldleft[Int, List[Int]](l, Nil)((a, b) => Cons(a + 1, b))
  }

  def inttostring(l: List[Double]): List[String] = {
    foldrightbyfoldleft[Double, List[String]](l, Nil)((a, b) => Cons(a.toString, b))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldrightbyfoldleft[A, List[B]](l, Nil)((a, b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldrightbyfoldleft[A, List[A]](as, Nil)((a, b) => {
      if (f(a)) b else Cons(a, b)
    })
  }

  def removeOdd(as: List[Int]) = {
    filter(as)(_ % 2 == 1)
  }

  def flatmap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldrightbyfoldleft[A, List[B]](l, Nil)((a, b) => append(f(a), b))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatmap(as)(a => {
      if (f(a)) List(a) else Nil
    })
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def matchFromStart(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (Nil, _) => true
        case (_, Nil) => true
        case (Cons(suph, supt), Cons(subh, subt)) => if (suph == subh) matchFromStart(supt, subt) else false
      }
    }

    (sup) match {
      case Nil => false
      case Cons(_, t) => if (matchFromStart(sup, sub)) true else hasSubsequence(t, sub)
    }

  }

  def main(args: Array[String]): Unit = {
    print(hasSubsequence(List(0,0,1,2,0,3,4,1,2,3,12,3),List(0,1,2,0,3,4)))
  }
}
