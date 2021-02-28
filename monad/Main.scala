trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def unit[A](a: => A): F[A]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((list, fa) =>
      flatMap(fa)(a => flatMap(list)(b => unit(a :: b)))
    )

  def sequence2[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](listOfA: List[A])(f: A => F[B]): F[List[B]] =
    listOfA.foldLeft(unit(List[B]()))((accum, a) =>
      flatMap(accum)((list) => flatMap(f(a))(b => unit(b :: list)))
    )

  def traverse2[A, B](listOfA: List[A])(f: A => F[B]): F[List[B]] =
    listOfA.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(predicate: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((a, fListOfA) =>
      map2(predicate(a), fListOfA)((boolean, listOfA) =>
        if (boolean)
          a :: listOfA
        else
          listOfA
      )
    )

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    flatMap(f(a))(g)

  def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def flatMap3[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def compose2[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

}

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))
}

case class State[S, A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newState) = run(s)
      f(a).run(newState)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))
}

object IntStateMonad
    extends Monad[({ type IntState[A] = State[Int, A] })#IntState] {
  def flatMap[A, B](st: State[Int, A])(f: A => State[Int, B]): State[Int, B] =
    st.flatMap(f)

  def unit[A](a: => A): State[Int, A] = State(s => (a, s))
}

object Main extends App {
  val optionMonad = new Monad[Option] {
    def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] =
      option.flatMap(f)

    def unit[A](a: => A): Option[A] = Some(a)
  }

  val listMonad = new Monad[List] {
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      as.flatMap((f))

    def unit[A](a: => A): List[A] = List(a)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)

    def unit[A](a: => A): State[S, A] = State(s => (a, s))
  }
}
