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

object Main extends App {}
