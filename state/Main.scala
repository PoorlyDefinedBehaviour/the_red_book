case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((state) => {
      val (a, s) = run(state)
      f(a).run(s)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap((a) => State.unit(f(a)))

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {
  def unit[S, A](a: A): State[S, A] = State((s) => (a, s))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(
      f: (A, B) => C
  ): State[S, C] =
    sa.flatMap((a) => sb.flatMap((b => State.unit(f(a, b)))))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(State.unit[S, List[A]](Nil))((a, state) =>
      State.map2(a, state)(_ :: _)
    )
}

object Main extends App {
  println(State((s: Int) => (s, s)).map(_ * 2).run(10))

  println(State.unit(1).map(_ + 1).run(1))

  println(State.unit(1).flatMap((x) => State.unit[Int, Int](x + 1)).run(3))

  println(
    State
      .sequence(
        List[State[Int, Int]](State.unit(1), State.unit(2), State.unit(3))
      )
      .run(0)
  )
}
