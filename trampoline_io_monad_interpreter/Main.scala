trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)((a: A) => (b: B) => f(a, b)))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)((a: A) => (b: B) => (c: C) => f(a, b, c)))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] =
    apply(
      apply(
        apply(map(fa)((a: A) => (b: B) => (c: C) => (d: D) => f(a, b, c, d)))(
          fb
        )
      )(fc)
    )(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fans: List[F[A]]): F[List[A]] =
    fans.foldRight(unit(List[A]()))((a, fbs) => map2(a, fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))
}

trait Monad[F[_]] extends Applicative[F] {
  def join[A](ffa: F[F[A]]): F[A] = flatMap((ffa))(fa => fa)

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(a => Return(f(a)))
}

case class Return[A](a: A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
case class Suspend[A](resume: () => A) extends IO[A]

object IO extends Monad[IO] {
  def apply[A, B](fab: IO[A => B])(fa: IO[A]): IO[B] =
    fab.flatMap(f => fa.map(a => f(a)))

  def unit[A](a: => A): IO[A] = Return(a)

  // tail recursive trampoline
  def run[A](io: IO[A]): A = io match {
    case Return(a)       => a
    case Suspend(resume) => resume()
    case FlatMap(sub, f) =>
      sub match {
        case Return(a)       => run(f(a))
        case Suspend(resume) => run(f(resume))
        case FlatMap(sub, g) => run(sub.flatMap(a => g(a).flatMap(f)))
      }
  }
}

object Main extends App {
  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  IO.run(printLine("hello world"))
}
