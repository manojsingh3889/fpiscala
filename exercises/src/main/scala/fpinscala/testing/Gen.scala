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

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop(
    (n: TestCases, rng: RNG) => (this.run(n, rng), p.run(n, rng)) match {
      case (Passed, Passed) => Passed
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1+f2, s1+s2)
      case (Passed, f) => f
      case (f, Passed) => f
    })
  def ||(p: Prop): Prop = Prop(
    (n: TestCases, rng: RNG) => (this.run(n, rng), p.run(n, rng)) match {
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1+f2, s1+s2)
      case (_, _) => Passed
    })
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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

object testPropAnd {
  def main(args: Array[String]): Unit = {
    val prop1 = Prop.forAll(Gen.choose(1, 10))(_ => false)
    val prop2 = Prop.forAll(Gen.boolean)(_ => true)
    println(prop1.&&(prop2).run(10, RNG.Simple(9)))
  }
}