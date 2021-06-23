package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
    case _ => empty
  }

  def takeWhile_1(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight[Option[A]](None)((h, _) => Some(h))
  }


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def appendLazy[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).append(t))
  }



  def toList: List[A] =
    this.foldRight[List[A]](List())(_ :: _)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipWithAll(s)((a,b) => if (b.isEmpty) return true else a==b).foldRight(true)(_ && _)

  //Stream((a,a2),(b,b2)(c,c2))

  def startsWith_1[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  //Implement tails using unfold. For a given Stream, tails returns the Stream of suf- fixes of the input sequence,
  // starting with the original Stream. For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  def tails: Stream[Stream[A]] = {
    unfold(this){
      case Cons(h,t) => Some(Cons(h,t), t())
      case _ => None
    } append(Stream(empty))
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }
}

object TestHeadOption {

  def main(args: Array[String]): Unit = {
    print(Stream.ones.headOption)
    print(Stream(10, 2, 3, 5).headOptionViaFoldRight)
  }
}

object TestMapViaFold {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3, 4).map(_ + 10.45).toList
    print(rel)
  }
}

object TestFilter {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3, 4).filter(_ % 2 == 1).toList
    print(rel)
    println()
    val rel2 = Stream(-1, -2, -3, -4).filter(_ % (-2) == (-1)).toList
    print(rel2)
  }
}

object TestAppend {
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val rel = Stream(1, 2, 3, 4).appendLazy(Stream(-1, -2, -3)).toList
    println(rel)
    println(System.currentTimeMillis() - start)
    another()
  }

  def another(): Unit = {
    val start = System.currentTimeMillis()
    val rel = Stream(1, 2, 3, 4).append(Stream(-1, -2, -3)).toList
    println(rel)
    println(System.currentTimeMillis() - start)
  }
}

object TestFlatMap {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3).flatMap(a => Stream(a * 10)).toList
    print(rel)
  }
}

object TestAll {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3).map(_ * 100).filter(_ % 15 == 0).append(Stream(-1, -2, -3)).flatMap(a => Stream(a + 1, a + 2)).toList
    print(rel)
  }
}

object TestDrop {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3, 4, 5, 2, 3, 4, 5).drop(4).toList
    assert(Stream(5, 2, 3, 4, 5).toList.equals(rel))
    print(rel)
  }
}

object TestTake {
  def main(args: Array[String]): Unit = {
    val rel = Stream(1, 2, 3, 4, 5, 2, 3, 4, 5).take(4).toList
    assert(Stream(1, 2, 3, 4).toList.equals(rel))
    print(rel)
  }
}

object TestTakeWhile {
  def main(args: Array[String]): Unit = {
    val result = Stream(2, 4, 6, 7, 8, 9).takeWhile(_ % 2 == 0).toList
    assert(result.equals(Stream(2, 4, 6).toList))
    println(result)
  }
}

object TestTakeWhile_1 {
  def main(args: Array[String]): Unit = {
    val result = Stream(2, 4, 6, 7, 8, 9).takeWhile_1(_ % 2 == 0).toList
    assert(result.equals(Stream(2, 4, 6).toList))
    println(result)
  }
}

object TestStartWith{
  def main(args: Array[String]): Unit = {
    val result = Stream(1,2,3,4).startsWith(Stream(1,2,3))
    assert(result)
    println (result)

    assert(!Stream(1,2,3,4).startsWith(Stream(1,2,3,5)))
    assert(!Stream(1,2).startsWith(Stream(1,2,3)))
    assert(Stream.empty.startsWith(Stream.empty))
  }
}

object TestTails{
  def main(args: Array[String]): Unit = {
    val list = Stream(1, 2, 3).tails.toList

    list.foreach{
      self => println(self.toList)
    }

    val expected = Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream()).toList
    println(list)
    println(expected)
  }
}

object Test{
  def main(args: Array[String]): Unit = {
    println(Integer.MIN_VALUE)
    println(Integer.MIN_VALUE.toDouble/Integer.MAX_VALUE)
  }

}