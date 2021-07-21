package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing.Gen.S
import fpinscala.testing.Prop._

import java.util.concurrent.Executors

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
case object Proved extends Result {
  def isFalsified = false
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop(
    (maxSize: MaxSize, n: TestCases, rng: RNG) => (this.run(maxSize, n, rng), p.run(maxSize, n, rng)) match {
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1+f2, s1+s2)
      case (Falsified(f1, s1), _) => Falsified(f1, s1)
      case (_, Falsified(f1, s1)) => Falsified(f1, s1)
      case (Passed, _) => Passed
      case (_, Passed) => Passed
      case (Proved, Proved) => Proved
    })

  def ||(p: Prop): Prop = Prop(
    (maxSize: MaxSize, n: TestCases, rng: RNG) => (this.run(maxSize, n, rng), p.run(maxSize, n, rng)) match {
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1+f2, s1+s2)
      case (Proved, _) => Proved
      case (_, Proved) => Proved
      case (Passed, Passed) => Passed
    })
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(i => listOfN(i, g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  val pint = Gen.choose(0,10) map (Par.unit(_))
  val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)
}

//trait Gen[A] {
case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(s => Gen(State.sequence(List.fill(s)(this.sample))))
  }

  def unsized: SGen[A] = {
    SGen(_ => this)
  }
}

//trait SGen[+A] {
//
//}


case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = {
    SGen(i => forSize(i).map(f))
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))
  }
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
    println(prop1.&&(prop2).run(10, 10, RNG.Simple(9)))
  }
}

object testListOf {
  def main(args: Array[String]): Unit = {
    println(Gen.listOf(Gen.choose(1, 10)).forSize.apply(10).sample.run(RNG.Simple(8)))
    println(Gen.listOf1(Gen.choose(1, 10)).forSize(0).sample.run(RNG.Simple(18)))
//    println(Gen.unit(1).listOfN(Gen.unit(10)).sample.run(RNG.Simple(8)))
//
//    println(Gen.listOfN(10, Gen.choose(1, 10)).sample.run(RNG.Simple(9)))
//    println(Gen.choose(1, 10).listOfN(Gen.unit(10)).sample.run(RNG.Simple(9)))
//
//    println(Gen.listOfN(10, Gen.boolean).sample.run(RNG.Simple(10)))
//    println(Gen.boolean.listOfN(Gen.unit(10)).sample.run(RNG.Simple(10)))
  }
}

object testSingletonListProp {
  def main(args: Array[String]): Unit = {
    println(Prop.forAll(
      Gen.listOf(Gen.choose(1, 10)).forSize(1)
    )(list => list.sorted == list).run(1, 1, RNG.Simple(8)))
  }
}

object testListMinMaxProp {
  def main(args: Array[String]): Unit = {
    println(Prop.forAll(
      Gen.listOf(Gen.choose(1, 10)).forSize(3)
    )(list => list.sorted.last == list.max && list.sorted.head == list.min).run(10, 10, RNG.Simple(8)))
  }
}

object testForkProp {
  def main(args: Array[String]): Unit = {
    val prop = Prop.forAllPar(Gen.pint2)(par => Par.equal(Par.fork(par), par))

    println(prop.run(10, 5, RNG.Simple(8)))
  }
}

object testTakeWhileProp {
  def main(args: Array[String]): Unit = {
    val properties: List[List[Int] => Boolean] = List(
      (l: List[Int]) => l.takeWhile(i => i >= 5) == l.dropWhile(i => i < 5),
      (l: List[Int]) => l.takeWhile(_ => true) == l.dropWhile(_ => false),
      (l: List[Int]) => l.takeWhile(_ => true) == l,
      (l: List[Int]) => l.takeWhile(_ => false) == List(),
      (l: List[Int]) => {
        val function: Int => Boolean = i => i >= 5
        l.takeWhile(function) ::: l.dropWhile(function) == l
      }
    )

    for (i <- properties.indices) {
      val prop = Prop.forAll(
        Gen.listOf(Gen.choose(1, 10)).forSize(3)
      )(properties(i))
      println(s"Prop $i: ${prop.run(10, 5, RNG.Simple(8))}")
    }
  }
}
