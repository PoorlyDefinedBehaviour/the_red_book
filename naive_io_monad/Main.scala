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

trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = f(self.run).run
    }

  def ++[B](io: IO[B]): IO[B] =
    new IO[B] {
      def run = {
        self.run;
        io.run
      }
    }
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
    fa.flatMap((f))

  def apply[A](a: => A): IO[A] = unit(a)
}

case class Player(name: String, score: Int)

object Main extends App {
  def readLine: IO[String] = IO { readLine }

  def printLine(message: String): IO[Unit] = IO { println(message) }

  def fahrenheitToCelsius(degrees: Double): Double = degrees

  def converter: IO[Unit] = for {
    _ <- printLine("Enter a temperature in degrees Fahrenheit: ")
    d <- readLine.map(_.toDouble)
    _ <- printLine(fahrenheitToCelsius(d).toString)
  } yield ()
}
