package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  val t = Branch[Int](
    Branch(
      Leaf(1),
      Branch(
        Branch(
          Leaf(11), Leaf(2)
        ),
        Leaf(3)
      )
    ),
    Branch(
      Leaf(2), Branch(
        Leaf(3), Branch(
          Leaf(5), Branch(
            Leaf(1), Branch(
              Leaf(123), Leaf(1234)
            )

          )
        )
      )
    )
  )

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum[A](t: Tree[A])(f: (A, A) => A): A = {
    t match {
      case Leaf(n) => n
      case Branch(l, r) => f(maximum(l)(f), maximum(r)(f))
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (1 + depth(l)).max(1 + depth(r))
    }
  }

  def map[A](t: Tree[A])(f: (A) => A): Tree[A] = {
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(fl: A => B)(fb: (B, B) => B): B = {
    t match {
      case Leaf(n) => fl(n)
      case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depthViaFold(t: Tree[Int]): Int = {
    fold(t)(_ => 1)((1+_ max 1+_))
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a=>Leaf(f(a)): Tree[B])(Branch(_,_))
  }

}

object TestSize {

  import Tree._

  def main(args: Array[String]): Unit = {
    printf(s"size: ${Tree.size(t)}")
    println()
    printf(s"size via fold: ${Tree.sizeViaFold(t)}")
  }
}

object TestMaximum {

  import Tree._

  def main(args: Array[String]): Unit = {
    printf("max: %d", Tree.maximum(t)(_ max _))
    println()
    printf("max via fold: %d", Tree.maximumViaFold(t))
  }
}

object TestDepth {

  import Tree._

  def main(args: Array[String]): Unit = {
    printf("depth: %d \ndepth via fold: %d", Tree.depth(t), Tree.depthViaFold(t))

  }
}

object TestMap {

  import Tree._

  def main(args: Array[String]): Unit = {
    val res = map[Int](t)(_ + 1)
    val res1 = mapViaFold(t)(_ + 1)
    printf(s"map: $res")
    println()
    printf(s"map via fold :$res1")
  }
}