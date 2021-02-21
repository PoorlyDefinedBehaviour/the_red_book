trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    flatMap(value => Some(f(value)))

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None        => None
      case Some(value) => f(value)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None        => default
      case Some(value) => value
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(value => Some(value)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(value => if (f(value)) Some(value) else None)
}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None
    }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def go(xs: List[Option[A]], ys: List[A]): Option[List[A]] =
      xs match {
        case Nil                 => Some(ys)
        case Some(value) :: tail => go(tail, value :: ys)
        case _                   => None
      }

    go(xs, List())
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def go(xs: List[A], ys: List[B]): Option[List[B]] =
      xs match {
        case Nil => Some(ys)
        case head :: tail =>
          f(head) match {
            case None        => None
            case Some(value) => go(tail, value :: ys)
          }
      }

    go(as, List())
  }

}

case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Math {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.length)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .flatMap(m => Math.mean(xs.map(x => math.pow(x - m, 2))))
}

object Main extends App {
  println(Math.mean(Seq()))
  println(Math.mean(Seq(1, 2, 3)))
  println(Some(1).map(_ * 2))
  println((None: Option[Int]).map(_ * 2))
  println(Some(3).flatMap(Some(_)))
  println((None: Option[Int]).flatMap(Some(_)))
  println(None.getOrElse(1))
  println(Some(3).getOrElse(1))
  println(None.orElse(Some(3)))
  println(Some(5).orElse(Some(3)))
  println(Some(3).filter(_ > 5))
  println((None: Option[Int]).filter(_ > 5))
  println(Math.variance(Seq(1, 2, 3)))
  println(Math.variance(Seq()))

  val abs0: Option[Double] => Option[Double] = Option.lift(math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Int = 1

  def parseInsuranceRateQuote(
      ageString: String,
      numberOfSpeedingTickets: String
  ): Option[Double] = {
    val age = Option.Try(ageString.toInt)
    val tickets = Option.Try(numberOfSpeedingTickets.toInt)
    Option.map2(age, tickets)(insuranceRateQuote)
  }

  println(Option.sequence(List(Some(1), Some(2), Some(3))))
  println(Option.sequence(List(Some(1), None, Some(3))))
  println(Option.traverse(List(1, 2, 3))((x) => Some(x * 2)))
  println(Option.traverse(List(1, 2, 3))((_) => None))
}
