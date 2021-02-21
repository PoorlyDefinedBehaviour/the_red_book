import scala.collection.Stepper
sealed trait Stream[+A] {
  def headOption: Option[A] =
    this match {
      case Empty         => None
      case Cons(head, _) => Some(head())
    }

  def toList: List[A] = {
    @annotation.tailrec
    def go(xs: Stream[A], ys: List[A]): List[A] =
      xs match {
        case Empty            => ys
        case Cons(head, tail) => go(tail(), ys.concat(List(head())))
      }

    go(this, List())
  }

  def take(n: Int): Stream[A] = {
    def go(index: Int, xs: Stream[A]): Stream[A] =
      xs match {
        case Empty                            => Empty
        case Cons(head, tail) if (index == n) => Empty
        case Cons(head, tail)                 => Stream.cons(head(), go(index + 1, tail()))
      }

    go(0, this)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(index: Int, xs: Stream[A]): Stream[A] =
      xs match {
        case Empty                => Empty
        case stream if index == n => stream
        case Cons(head, tail)     => go(index + 1, tail())
      }

    go(0, this)
  }

  def takeWhile(predicate: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(head, tail) =>
        if (predicate(head()))
          Stream.cons(head(), tail().takeWhile(predicate))
        else
          Empty
    }

  def exists(predicate: A => Boolean): Boolean =
    this match {
      case Cons(head, tail) => predicate(head()) || tail().exists(predicate)
      case _                => false
    }

  def foldRight[B](emptyValue: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(head, tail) => f(head(), tail().foldRight(emptyValue)(f))
      case _                => emptyValue
    }

  def exists2(predicate: A => Boolean): Boolean =
    foldRight(false)((a, b) => predicate(a) || b)

  def forall(predicate: A => Boolean): Boolean =
    foldRight(false)((a, b) => predicate(a) && b)

  def forall2(predicate: A => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(head, tail) =>
        if (predicate(head()))
          tail().forall2(predicate)
        else
          false
    }

  def takeWhile2(predicate: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (predicate(a))
        Stream.cons(a, b)
      else
        Empty
    )

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(predicate: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (predicate(a))
        Stream.cons(a, b)
      else
        b
    )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) =>
      f(a) match {
        case Empty         => Empty
        case Cons(head, _) => Stream.cons(head(), b)
      }
    )

  def append[B >: A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((a, b) => Stream.cons(a, b))

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)((stream) =>
      stream match {
        case Empty            => None
        case Cons(head, tail) => Some((f(head()), tail()))
      }
    )

  def take2(n: Int): Stream[A] =
    Stream.unfold((0, this))((values) => {
      val (index, stream) = values

      stream match {
        case Empty            => None
        case _ if index == n  => None
        case Cons(head, tail) => Some((head(), (index + 1, tail())))
      }
    })

  def takeWhile3(predicate: A => Boolean): Stream[A] =
    Stream.unfold(this)((stream) =>
      stream match {
        case Empty => None
        case Cons(head, tail) => {
          val h = head()

          if (predicate(h))
            Some((h, tail()))
          else
            None
        }

      }
    )

  def zipAll[B](streamB: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, streamB))((streams) =>
      streams match {
        case (Empty, Empty) => None
        case (Cons(headA, tailA), Cons(headB, tailB)) =>
          Some(((Some(headA()), Some(headB())), (tailA(), tailB())))
        case (Cons(headA, tailA), Empty) =>
          Some((Some(headA()), None), (tailA(), Empty))
        case (Empty, Cons(headB, tailB)) =>
          Some(((None, Some(headB())), (Empty, tailB())))
      }
    )

  def startsWith[B >: A](stream: Stream[B]): Boolean = {
    @annotation.tailrec
    def go(xs: Stream[A], ys: Stream[B]): Boolean =
      (xs, ys) match {
        case (_, Empty) => true
        case (Empty, _) => false
        case (Cons(headA, tailA), Cons(headB, tailB)) =>
          if (headA() == headB())
            go(tailA(), tailB())
          else
            false
      }

    go(this, stream)
  }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)((stream) =>
      stream match {
        case Empty            => None
        case Cons(head, tail) => Some((Stream.cons(head(), tail()), tail()))
      }
    )

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](initialValue: B)(f: (A, B) => B): Stream[B] =
    foldRight((initialValue, Stream(initialValue)))((a, accumAndStream) => {
      val (accum, stream) = accumAndStream
      val b = f(a, accum)
      (b, Stream.cons(b, stream))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, Stream.constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, Stream.from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None                 => Stream.empty
      case Some((value, state)) => Stream.cons(value, unfold(state)(f))
    }

  def from2(n: Int): Stream[Int] =
    unfold(n)((x) => Some(x, x + 1))

  def zipWith[A, B, C](streamA: Stream[A], streamB: Stream[B])(
      f: => (A, B) => C
  ): Stream[C] =
    Stream.unfold((streamA, streamB))((streams) =>
      streams match {
        case (Empty, _) => None
        case (_, Empty) => None
        case (Cons(headA, tailA), Cons(headB, tailB)) =>
          Some((f(headA(), headB()), (tailA(), tailB())))
      }
    )
}

object Main extends App {
  val ones: Stream[Int] = Stream.cons(1, ones)
  val twos: Stream[Int] = Stream.constant(2)
  val ones2: Stream[Int] = Stream.unfold(1)((_) => Some((1, 1)))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))

    go(0, 1)
  }

  def fibs2: Stream[Int] =
    Stream.unfold((0, 1))((values) => {
      val (a, b) = values
      Some((a, (b, a + b)))
    })

  println(Stream(1, 2, 3).toList)
  println(Stream(1, 2, 3, 4, 5).take(3).toList)
  println(Stream(1, 2, 3, 4, 5).drop(2).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile2(_ < 3).toList)
  println(Stream(1, 2, 3).headOption2)
  println(Stream().headOption2)
  println(Stream(1, 2, 3).map(_ * 2).toList)
  println(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList)
  println(Stream(1, 2, 3).flatMap(Stream(_)).toList)
  println(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList)
  println(Stream(1, 2, 3).append(Stream.empty).toList)
  println(Stream.empty.append(Stream(4, 5, 6)).toList)
  println(ones.take(5).toList)
  println(twos.take(5).toList)
  println(Stream.from(10).take(5).toList)
  println(fibs.take(10).toList)
  println(Stream.unfold(1)((x) => Some((x, x + 2))).take(10).toList)
  println(ones2.take(5).toList)
  println(Stream.from2(10).take(5).toList)
  println(fibs2.take(10).toList)
  println(Stream(1, 2, 3).map2(_ * 2).toList)
  println(Stream(1, 2, 3, 4, 5).take2(3).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile3(_ < 3).toList)
  println(Stream.zipWith(Stream(1, 2, 3), Stream(4, 5, 6))(_ + _).toList)
  println(Stream(1, 2, 3).zipAll(Stream(4, 5)).toList)
  println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  println(Stream(1, 2, 3).startsWith(Stream()))
  println(Stream(1, 2, 3).startsWith(Stream(1, 2, 3)))
  println(Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4, 5)))
  println(Stream(1, 2, 3).tails.map(_.toList).toList)
  println(Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3, 4)))
  println(Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3, 6)))
  println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
}
