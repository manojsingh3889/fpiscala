package fpinscala.state

import fpinscala.state.RNG.{Simple, int, unit}


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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i == Int.MinValue)
      return nonNegativeInt(r)
    (Math.abs(i), r)
  }

  def nonNegativeInt_2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    if (i == Int.MaxValue)
      return double(r)
    (i.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def map2_withFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]]((Nil, _))(map2(_, _)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      g(a)(r2)
    }

  def boolean(rng: RNG): (Boolean, RNG) = RNG.nonNegativeInt(rng) match {
    case (i, r) => (i % 2 == 0, r)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      sa => {
        val (a, sa1) = run(sa)
        val (b, sb1) = sb.run(sa1)
        (f(a, b), sb1)
      }
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s2) = run(s)
        f(a).run(s2)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight[State[S, List[A]]](State((Nil, _)))((h, t) => h.map2(t)(_ :: _))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}

object TestUnit {
  def main(args: Array[String]): Unit = {
    println(RNG.unit(10)(Simple(0)))
    println(State.unit(10).run(Simple(0)))
  }
}

object TestMap {
  def main(args: Array[String]): Unit = {
    println(RNG.map(int)(_ + 11)(Simple(11)))
    println(State(int).map(_ + 11).run(Simple(11)))
  }
}

object TestMap2 {
  def main(args: Array[String]): Unit = {
    println(RNG.map2(int, int)(_ + _)(Simple(10)))

    println(State(int).map2(State(int))(_ + _).run(Simple(10)))
  }
}

object TestFlatMap {
  def main(args: Array[String]): Unit = {
    println(RNG.flatMap(int)(i => unit(1 + i))(Simple(2)))

    println(State(int).flatMap(i => State.unit(1 + i)).run(Simple(2)))
  }
}

object TestSequence{
  def main(args: Array[String]): Unit = {
    println(RNG.sequence(List(int, int, int))(Simple(10)))

    println(State.sequence(List(State(int), State(int), State(int))).run(Simple(10)))
  }
}
