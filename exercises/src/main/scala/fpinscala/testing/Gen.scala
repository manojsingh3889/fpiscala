package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))
  def double: Gen[Double] =
    Gen(State(RNG.double))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen.boolean.flatMap(flag => if(flag) g1 else g2)
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val ratio = g1._2/(g1._2 + g2._2)
    Gen.double.flatMap(p => if(p < ratio) g1._1 else g2._1)
  }
}

//trait Gen[A] {
case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(s => Gen(State.sequence(List.fill(s)(this.sample))))
  }
}

trait SGen[+A] {

}


object TestBoolean {
  def main(args: Array[String]): Unit = {
    println(Gen.boolean.sample.run(RNG.Simple(8)))
    println(Gen.boolean.sample.run(RNG.Simple(9)))
    println(Gen.boolean.sample.run(RNG.Simple(10)))
    println(Gen.boolean.sample.run(RNG.Simple(11)))
  }
}
object TestListOfN {
  def main(args: Array[String]): Unit = {
    println(Gen.listOfN(10, Gen.unit(1)).sample.run(RNG.Simple(8)))
    println(Gen.unit(1).listOfN(Gen.unit(10)).sample.run(RNG.Simple(8)))

    println(Gen.listOfN(10, Gen.choose(1, 10)).sample.run(RNG.Simple(9)))
    println(Gen.choose(1, 10).listOfN(Gen.unit(10)).sample.run(RNG.Simple(9)))

    println(Gen.listOfN(10, Gen.boolean).sample.run(RNG.Simple(10)))
    println(Gen.boolean.listOfN(Gen.unit(10)).sample.run(RNG.Simple(10)))
  }
}

object testGenMap {
  def main(args: Array[String]): Unit = {
    println(Gen.boolean.sample.map({ a => !a }).run(RNG.Simple(6)))
    println(Gen.boolean.flatMap({ a => Gen.unit(!a) }).sample.run(RNG.Simple(6)))
    println(Gen.boolean.sample.run(RNG.Simple(6)))
  }
}

object testUnion {
  def main(args: Array[String]): Unit = {
    println(Gen.union(Gen.choose(1, 5),Gen.choose(6, 15)).sample.run(RNG.Simple(4)))
    println(Gen.union(Gen.choose(1, 5),Gen.choose(6, 15)).sample.run(RNG.Simple(5)))
  }
}

object testWeighted {
  def main(args: Array[String]): Unit = {
    var rng: RNG = RNG.Simple(6)
    for (_ <- 1 to 20) {
      println(Gen.weighted((Gen.unit(0),2),(Gen.unit(1), 18)).sample.run(rng))
      rng = rng.nextInt._2
    }
  }
}