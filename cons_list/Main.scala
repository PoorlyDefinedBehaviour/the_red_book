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

  def flatten[A](list: List[List[A]]): List[A] = {
    @annotation.tailrec
    def go(list: List[List[A]], flatList: List[A]): List[A] =
      list match {
        case Nil              => flatList
        case Cons(head, tail) => go(tail, List.append(flatList, head))
      }

    go(list, Nil)
  }

  def map[A, B](listOfA: List[A])(f: A => B): List[B] =
    foldLeft2(listOfA, Nil: List[B])((x, listOfB) =>
      List.append(listOfB, List(f(x)))
    )

  def inc(list: List[Int]): List[Int] = map(list)(_ + 1)

  def doublesToString(list: List[Double]): List[String] =
    map(list)(_.toString)

  def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
    foldLeft(list, Nil: List[A])((x, filteredList) =>
      if (predicate(x))
        List.append(filteredList, List(x))
      else
        filteredList
    )

  def flatMap[A, B](listOfA: List[A])(f: A => List[B]): List[B] =
    foldLeft2(listOfA, Nil: List[B])((x, listOfB) => List.append(listOfB, f(x)))

  def filter2[A](list: List[A])(predicate: A => Boolean): List[A] =
    flatMap(list)(x =>
      if (predicate(x))
        List(x)
      else
        Nil
    )

  def zipWith[A, B, C](listOfA: List[A], listOfB: List[B])(
      f: (A, B) => C
  ): List[C] = {
    @annotation.tailrec
    def go(listOfA: List[A], listOfB: List[B], listOfC: List[C]): List[C] =
      (listOfA, listOfB) match {
        case (Nil, _) | (_, Nil) => listOfC
        case (Cons(headA, tailA), Cons(headB, tailB)) =>
          go(tailA, tailB, List.append(listOfC, List(f(headA, headB))))
      }

    go(listOfA, listOfB, Nil: List[C])
  }

  def take[A](n: Int, list: List[A]): List[A] = {
    @annotation.tailrec
    def go(list: List[A], index: Int, xs: List[A]): List[A] =
      list match {
        case Nil             => xs
        case _ if index == n => xs
        case Cons(head, tail) =>
          go(tail, index + 1, List.append(xs, List(head)))
      }

    go(list, 0, Nil: List[A])
  }

  def takeWhile[A](list: List[A])(predicate: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(list: List[A], xs: List[A]): List[A] =
      list match {
        case Nil => xs
        case Cons(head, tail) =>
          if (predicate(head))
            go(tail, List.append(xs, List(head)))
          else
            xs
      }

    go(list, Nil: List[A])
  }

  @annotation.tailrec
  def forall[A](list: List[A])(predicate: A => Boolean): Boolean =
    list match {
      case Nil => true
      case Cons(head, tail) =>
        if (predicate(head))
          forall(tail)(predicate)
        else
          false
    }

  @annotation.tailrec
  def some[A](list: List[A])(predicate: A => Boolean): Boolean =
    list match {
      case Nil => false
      case Cons(head, tail) =>
        if (predicate(head))
          true
        else
          some(tail)(predicate)
    }

  def scanLeft[A, B](listOfA: List[A], initialValue: B)(
      f: (A, B) => B
  ): List[B] = {
    def reduceAndAppend(x: A, tuple: (B, List[B])): (B, List[B]) = {
      val (reducedValue, listOfReducedValues) = tuple
      val result = f(x, reducedValue)
      (result, List.append(listOfReducedValues, List(result)))
    }

    val (_, reducedValues) =
      foldLeft2(listOfA, (initialValue, Nil: List[B]))(reduceAndAppend)

    reducedValues
  }

  def scanRight[A, B](listOfA: List[A], initialValue: B)(
      f: (A, B) => B
  ): List[B] = {
    def reduceAndAppend(x: A, tuple: (B, List[B])): (B, List[B]) = {
      val (reducedValue, listOfReducedValues) = tuple
      val result = f(x, reducedValue)
      (result, Cons(result, listOfReducedValues))
    }

    val (_, reducedValues) =
      foldRight(listOfA, (initialValue, Nil: List[B]))(reduceAndAppend)

    reducedValues
  }

  def isEmpty[A](list: List[A]): Boolean =
    list match {
      case Nil => true
      case _   => false
    }

  def hasSubsequence[A](xs: List[A], ys: List[A]): Boolean = {
    if (List.isEmpty(xs) || List.isEmpty(ys)) {
      return false
    }

    def elementsMatch(xs: List[A], ys: List[A]): Boolean =
      (xs, ys) match {
        case (Nil, Nil)        => true
        case (_, Nil)          => true
        case (Nil, Cons(_, _)) => false
        case (Cons(headX, tailX), Cons(headY, tailY)) =>
          if (headX != headY)
            false
          else
            elementsMatch(tailX, tailY)
      }

    @annotation.tailrec
    def go(xs: List[A]): Boolean =
      xs match {
        case Nil => false
        case Cons(head, tail) =>
          if (head == List.head(ys) && elementsMatch(tail, List.tail(ys)))
            true
          else
            go(tail)
      }

    go(xs)
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
  println(List.flatten(List(List(1, 2, 3))))
  println(List.inc(List(1, 2, 3)))
  println(List.doublesToString(List(1.0, 2.0, 3.0)))
  println(List.map(List(1, 2, 3))(_ * 2))
  println(List.filter(List(1, 2, 3, 4, 5))((x: Int) => (x & 1) == 0))
  println(List.flatMap(List(1, 2, 3))(x => List(x, x)))
  println(List.filter2(List(1, 2, 3, 4, 5))((x: Int) => (x & 1) == 0))
  println(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
  println(List.take(3, List(1, 2, 3, 4, 5)))
  println(List.takeWhile(List(1, 2, 3, 4, 5))(_ != 3))
  println(List.forall(List(1, 2, 3))(_ > 0))
  println(List.forall(List(1, 2, 3))(_ < 0))
  println(List.some(List(1, 2, 3))(_ == 3))
  println(List.some(List(1, 2, 3))(_ == 4))
  println(List.scanLeft(List(1, 2, 3), 0)(_ + _))
  println(List.scanRight(List(1, 2, 3), 0)(_ + _))
  println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3)))
  println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(5)))
  println(List.hasSubsequence(List(1, 2, 3, 4, 5), List()))
  println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)))
  println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4)))
}
