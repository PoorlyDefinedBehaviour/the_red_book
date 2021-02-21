package main

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone(): Boolean = true
    def get(timeout: Long, units: TimeUnit): A = ???
    def isCancelled(): Boolean = false
    def cancel(evenifRunning: Boolean): Boolean = false
  }

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(
      f: (A, B, C) => D
  ): Par[D] = {
    val xy = map2(a, b)((x, y) => (x, y))
    map2(xy, c)((xy, z) => f(xy._1, xy._2, z))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(
      f: (A, B, C, D) => E
  ): Par[E] = {
    val xyz = map3(a, b, c)((x, y, z) => (x, y, z))
    map2(xyz, d)((xyz, w) => f(xyz._1, xyz._2, xyz._3, w))
  }

  def map5[A, B, C, D, E, F](
      a: Par[A],
      b: Par[B],
      c: Par[C],
      d: Par[D],
      e: Par[E]
  )(
      f: (A, B, C, D, E) => F
  ): Par[F] = {
    val abcd = map4(a, b, c, d)((a, b, c, d) => (a, b, c, d))
    map2(abcd, e)((abcd, e) => f(abcd._1, abcd._2, abcd._3, abcd._4, e))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val pars = ps.map(asyncF(f))
    sequence(pars)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val pars = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

  def equal[A](e: ExecutorService)(parA: Par[A], parB: Par[A]): Boolean =
    parB(e).get == parB(e).get

  def delay[A](fa: => Par[A]): Par[A] = (es: ExecutorService) => fa(es)

  def run[A](a: Par[A]): A = ???
}

object Main extends App {
  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (left, right) = ints.splitAt(ints.length / 2)
      val sumLeft = Par.unit(sum(left))
      val sumRight = Par.unit(sum(right))
      Par.run(sumLeft) + Par.run(sumRight)
    }
  }
}
