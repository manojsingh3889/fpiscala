package fpinscala.parallelism

import fpinscala.parallelism.Par.choiceN

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

//  private case class PairFuture[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
////    var result
//    def isDone = a.isDone && b.isDone
//    def get(timeout: Long, units: TimeUnit) = {
////      Long start = System.nanoTime()
//      val av = a.get(timeoutInNanoSecs, TimeUnit.NANOSECONDS)
//      val bv = b.get(timeoutInNanoSecs - System.nanoTime + start, TimeUnit.NANOSECONDS)
//      f(av, bv)
//    }
//    def isCancelled = a.isCancelled && b.isCancelled
//    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
//  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts,
      // and eagerly waits for the returned futures.
      // This means that even if you have passed in "forked" arguments,
      // using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them,
      // and wraps them in a `UnitFuture`. In order to respect timeouts,
      // we'd need a new `Future` implementation that records the amount of time spent evaluating
      // `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

//  def map2WithTimeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
//      PairFuture(af, bf, f) // This implementation of `map2` does _not_ respect timeouts,
//      // and eagerly waits for the returned futures.
//      // This means that even if you have passed in "forked" arguments,
//      // using this map2 on them will make them wait.
//      // It simply passes the `ExecutorService` on to both `Par` values,
//      // waits for the results of the Futures `af` and `bf`, applies `f` to them,
//      // and wraps them in a `UnitFuture`. In order to respect timeouts,
//      // we'd need a new `Future` implementation that records the amount of time spent evaluating
//      // `af`, then subtracts that time from the available time allocated for evaluating `bf`.
//    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceCombinator[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      combinator[A, Boolean](cond)(b => {
        if(b) t
        else f
      })

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      choices(run(es)(n).get())(es)

  def choiceNCombinator[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =

      combinator[A, Int](n) (choices(_))


  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es =>
      choices(run(es)(key).get())(es)

  def choiceMapCombinator[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    combinator[V, K](key) (choices(_))

  def combinator[B, C](pc: Par[C])(f: C => Par[B]): Par[B] =
    es =>
      f(run(es)(pc).get())(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

object TestChoiceN {
  def main(args: Array[String]): Unit = {
    val n = Par.asyncF[Int, Int]( _ => {
      println(Thread.currentThread().getName)
      2
    })
    val es = Executors.newSingleThreadExecutor()
    try {
      println(Par.run(es)(Par.choiceN(n.apply(1))(List(Par.lazyUnit(1), Par.unit(2), Par.unit(3)))).get())
    } catch {
      case ex: Throwable => println(ex.toString)
    } finally {
      es.shutdown()
    }
  }
}

object TestChoiceMap {
  def main(args: Array[String]): Unit = {
    val n = Par.asyncF[Int, Int]( _ => {
      println(Thread.currentThread().getName)
      2
    })
    val es = Executors.newSingleThreadExecutor()
    try {
      println(Par.run(es)(Par.choiceMap(n.apply(1))(Map(1 -> Par.lazyUnit(1), 2 -> Par.unit(2), 3 -> Par.unit(3)))).get())
    } catch {
      case ex: Throwable => println(ex.toString)
    } finally {
      es.shutdown()
    }
  }
}

object TestChoiceMapCombinator {
  def main(args: Array[String]): Unit = {
    val n = Par.asyncF[Int, Int]( _ => {
      println(Thread.currentThread().getName)
      2
    })
    val es = Executors.newSingleThreadExecutor()
    try {
      println(Par.run(es)(Par.choiceMapCombinator(n.apply(1))(Map(1 -> Par.lazyUnit(1), 2 -> Par.unit(2), 3 -> Par.unit(3)))).get())
    } catch {
      case ex: Throwable => println(ex.toString)
    } finally {
      es.shutdown()
    }
  }
}