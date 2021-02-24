trait Parsers[ParseError, Parser[+_]] {
  self =>

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  def or[A](parserA: Parser[A], parserB: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def label[A](message: String)(p: Parser[A]): Parser[A]

  def scope[A](message: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())

    def many1[A](p: Parser[A]): Parser[List[A]]

    def product[A, B](
        parserA: Parser[A],
        parserB: => Parser[B]
    ): Parser[(A, B)] =
      flatMap(parserA)(a =>
        parserB(a)
          .flatMap(b => succeed((a, b)))
      )

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      flatMap(p)(a => succeed(f(a)))

    def map2[A, B, C](parserA: Parser[A], parserB: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] =
      flatMap(parserA)(a =>
        flatMap(parserB)
          .flatMap(b => succed(f(a, b)))
      )

    def succeed[A](a: A): Parser[A] = string("").map(_ => a)

    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }
  }

  case class ParseError(stack: List[(Location, String)])

  def errorLocation(e: ParseError): Location
  def errorMEesage(e: ParseError): String
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object JSONParser {
  def parse[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice
  }
}

object Mains extends App {
  println("hello world")
}
