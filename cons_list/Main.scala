sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def drop[A](n: Int, xs: List[A]): List[A] = {
    if (n < 0) {
      throw new Exception("n can't be negative")
    }

    @annotation.tailrec
    def go(i: Int, ys: List[A]): List[A] = {
      if (i >= n) {
        return ys
      }

      ys match {
        case Nil         => Nil
        case Cons(_, ys) => go(i + 1, ys)
      }

    }

    go(0, xs)
  }

  def dropWhile[A](xs: List[A], predicate: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(ys: List[A]): List[A] = {
      val head = List.head(ys)

      if (predicate(head)) {
        go(List.tail(ys))
      } else {
        ys
      }
    }

    go(xs)
  }

  def head[A](xs: List[A]): A = xs match {
    case Nil =>
      throw new Exception("head called on empty list")
    case Cons(x, _) => x
  }

  def tail[A](xs: List[A]): List[A] = drop(1, xs)

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil              => Nil
    case Cons(x, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def setHead[A](x: A, xs: List[A]): List[A] = Cons(x, tail(xs))

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil              => ys
    case Cons(head, tail) => Cons(head, append(tail, ys))
  }

  def foldRight[A, B](xs: List[A], accum: B)(f: (A, B) => B): B =
    xs match {
      case Nil              => accum
      case Cons(head, tail) => f(head, foldRight(tail, accum)(f))
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double =
    foldRight(doubles, 1.0)(_ * _)

  def apply[A](xs: A*): List[A] = {
    if (xs.isEmpty) {
      Nil
    } else {
      Cons(xs.head, apply(xs.tail: _*))
    }
  }
}

object Main extends App {
  println(List.init(List(1, 2, 3, 4)))
  println(List.append(List(1, 2, 3), List(4, 5, 6)))
  println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4))
  println(List.sum(List(1, 1, 1)))
  println(List.drop(2, List(1, 2, 3)))
  println(List.tail(List(1, 2, 3)))
  println(List.product(List(1, 2, 3)))
  println(List.setHead(5, List(1, 2, 3)))
}
