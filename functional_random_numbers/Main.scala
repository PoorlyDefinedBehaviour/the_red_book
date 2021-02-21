sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Main extends App {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt

    val positiveN = if (n == Int.MinValue) {
      Int.MaxValue
    } else {
      math.abs(n)
    }

    (positiveN, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (doubleN, newRng) = nonNegativeInt(rng)
    (doubleN / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    (nonNegativeInt(rng), double(rng)) match {
      case ((i, _), (d, newRng)) => ((i, d), newRng)
    }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    intDouble(rng) match {
      case ((i, d), newRng) => ((d, i), newRng)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    (double(rng), double(rng), double(rng)) match {
      case ((d1, _), (d2, _), (d3, newRng)) => ((d1, d2, d3), newRng)
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(
        currentCount: Int,
        currentRng: RNG,
        xs: List[Int]
    ): (List[Int], RNG) =
      currentRng.nextInt match {
        case (i, newRng) =>
          if (currentCount < count)
            go(currentCount + 1, newRng, i :: xs)
          else
            (i :: xs, newRng)
      }

    go(0, rng, List())
  }

  println(nonNegativeInt(SimpleRNG(100)))
  println(nonNegativeInt(SimpleRNG(3)))
  println(nonNegativeInt(SimpleRNG(42)))

  println(double(SimpleRNG(100)))
  println(double(SimpleRNG(3)))
  println(double(SimpleRNG(42)))

  println(intDouble(SimpleRNG(3)))

  println(doubleInt(SimpleRNG(3)))

  println(double3(SimpleRNG(3)))

  println(ints(10)(SimpleRNG(3)))

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  println(nonNegativeInt(SimpleRNG(3)))

  def double2: Rand[Double] =
    map(nonNegativeEven)(i => i / (Int.MaxValue.toDouble + 1))

  println(double2(SimpleRNG(3)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((a, accum) => map2(a, accum)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapp[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)((x) => unit(f(x)))

  def mapp2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)((a) => flatMap(rb)((b) => unit(f(a, b))))
}
