trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    flatMap((x) => Right(f(x)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(value) => f(value)
      case Left(error)  => Left(error)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)      => b
      case Right(value) => Right(value)
    }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try {
      Right(a)
    } catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = {
    @annotation.tailrec
    def go(xs: List[Either[E, A]], ys: List[A]): Either[E, List[A]] =
      xs match {
        case Left(error) :: _     => Left(error)
        case Right(value) :: tail => go(tail, value :: ys)
        case _                    => Right(ys)
      }

    go(list, List())
  }

  def traverse[E, A, B](
      list: List[A]
  )(f: A => Either[E, B]): Either[E, List[B]] = {
    @annotation.tailrec
    def go(xs: List[A], ys: List[B]): Either[E, List[B]] =
      xs match {
        case Nil => Right(ys)
        case head :: tail =>
          f(head) match {
            case Left(error) => Left(error)
            case Right(y)    => go(tail, y :: ys)
          }
      }

    go(list, List())
  }

  def map2[E, A, B, C](ea: Either[E, A], eb: Either[E, B])(
      f: (A, B) => C
  ): Either[E, C] =
    ea.flatMap((a) => eb.map((b => f(a, b))))
}

object Name {
  def make(name: String): Either[String, Name] =
    if (name.isEmpty || name == null)
      Left("Name is empty.")
    else
      Right(new Name(name))
}

object Age {
  def make(age: Int): Either[String, Age] =
    if (age < 0)
      Left("Age is out of range.")
    else
      Right(new Age(age))
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def make(name: String, age: Int): Either[String, Person] =
    Either.map2(Name.make(name), Age.make(age))(Person(_, _))
}

object Main extends App {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def saveDif(x: Int, y: Int): Either[Exception, Int] =
    Either.Try(x / y)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Int = 1

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String
  ): Either[Exception, Double] =
    for {
      a <- Either.Try(age.toInt)
      tickets <- Either.Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)

  println("hello world")
}
