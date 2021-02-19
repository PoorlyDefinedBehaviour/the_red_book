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

  def foldLeft[A, B](xs: List[A], accum: B)(f: (A, B) => B): B =
    xs match {
      case Nil              => accum
      case Cons(head, tail) => foldLeft(tail, f(head, accum))(f)
    }

  def foldLeft2[A, B](xs: List[A], accum: B)(f: (A, B) => B): B = {
    @annotation.tailrec
    def go(xs: List[A], accum: B): B = xs match {
      case Nil              => accum
      case Cons(head, tail) => go(tail, f(head, accum))
    }

    go(xs, accum)
  }

  def length[A](xs: List[A]): Int = {
    @annotation.tailrec
    def go(xs: List[A], len: Int): Int = xs match {
      case Nil               => len
      case Cons(_head, tail) => go(tail, len + 1)
    }

    go(xs, 0)
  }

  def length2[A](xs: List[A]): Int =
    foldRight(xs, 0)((_, len) => len + 1)

  def length3[A](xs: List[A]): Int =
    foldLeft(xs, 0)((_, len) => len + 1)

  def length4[A](xs: List[A]): Int = foldLeft2(xs, 0)((_, len) => len + 1)

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def sum3(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double =
    foldRight(doubles, 1.0)(_ * _)

  def product3(doubles: List[Double]): Double =
    foldLeft(doubles, 1.0)(_ * _)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((y, ys) => Cons(y, ys))

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
  println(
    List.length(List(1, 2, 3))
  )
  println(List.length(List()))
  println(List.length(List(1)))
  println(
    List.length2(List(1, 2, 3))
  )
  println(List.length2(List()))
  println(List.length2(List(1)))
  println(
    List.length3(List(1, 2, 3))
  )
  println(List.length3(List()))
  println(List.length3(List(1)))
  println(
    List.length4(List(1, 2, 3))
  )
  println(List.length4(List()))
  println(List.length4(List(1)))
  println(List.reverse(List(1, 2, 3)))
}
