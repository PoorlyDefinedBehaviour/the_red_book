package parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executor
import java.util.concurrent.Callable
import scala.actors._

sealed trait Future[+A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

object Par {
  type Par[+A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    _es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

  def fork[A](par: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit = eval(es)(par(es)(cb))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              br match {
                case None    => ar = Some(a)
                case Some(b) => eval(es)(cb(f(a, b)))
              }
            case Right(b) =>
              ar match {
                case None    => br = Some(b)
                case Some(a) => eval(es)(cb(f(a, b)))
              }
          }

          parA(es)(a => combiner ! Left(a))
          parB(es)(b => combiner ! Right(b))
        }
      }

  def choiceBlocking[A](
      condition: Par[Boolean]
  )(Then: Par[A], Else: Par[A]): Par[A] =
    es =>
      if (run(es)(condition))
        Then(es)
      else
        Else(es)

  def run[A](es: ExecutorService)(par: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    par(es)((a) => {
      ref.set(a)
      latch.countDown()
    })

    latch.await()

    ref.get()
  }
}

object Main extends App {}
