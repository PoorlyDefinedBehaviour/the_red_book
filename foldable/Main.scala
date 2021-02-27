trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
}

object Monoid {
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a: (A, B), b: (A, B)): (A, B) =
        (ma.op(a._1, b._1), mb.op(a._2, b._2))

      def zero: (A, B) = (ma.zero, mb.zero)
    }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero)((map, key) => {
          val combinedValue =
            v.op(a.getOrElse(key, v.zero), b.getOrElse(key, v.zero))

          map.updated(key, combinedValue)
        })

      def zero: Map[K, V] = Map[K, V]()

    }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B =
        a => mb.op(f(a), g(a))

      def zero: A => B = (_) => mb.zero
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as.foldLeft(Map[A, Int]())((map, value) => {
      val valueAppearanceCount = map.getOrElse(value, 0) + 1

      map.updated(value, valueAppearanceCount)
    })
}

sealed trait Stream[+A] {
  def headOption: Option[A] =
    this match {
      case Empty               => None
      case StreamCons(head, _) => Some(head())
    }

  def toList: List[A] = {
    @annotation.tailrec
    def go(xs: Stream[A], ys: List[A]): List[A] =
      xs match {
        case Empty                  => ys
        case StreamCons(head, tail) => go(tail(), ys.concat(List(head())))
      }

    go(this, List())
  }

  def take(n: Int): Stream[A] = {
    def go(index: Int, xs: Stream[A]): Stream[A] =
      xs match {
        case Empty                                  => Empty
        case StreamCons(head, tail) if (index == n) => Empty
        case StreamCons(head, tail) =>
          Stream.cons(head(), go(index + 1, tail()))
      }

    go(0, this)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(index: Int, xs: Stream[A]): Stream[A] =
      xs match {
        case Empty                  => Empty
        case stream if index == n   => stream
        case StreamCons(head, tail) => go(index + 1, tail())
      }

    go(0, this)
  }

  def takeWhile(predicate: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case StreamCons(head, tail) =>
        if (predicate(head()))
          Stream.cons(head(), tail().takeWhile(predicate))
        else
          Empty
    }

  def exists(predicate: A => Boolean): Boolean =
    this match {
      case StreamCons(head, tail) =>
        predicate(head()) || tail().exists(predicate)
      case _ => false
    }

  def foldRight[B](emptyValue: => B)(f: (A, => B) => B): B =
    this match {
      case StreamCons(head, tail) => f(head(), tail().foldRight(emptyValue)(f))
      case _                      => emptyValue
    }

  def exists2(predicate: A => Boolean): Boolean =
    foldRight(false)((a, b) => predicate(a) || b)

  def forall(predicate: A => Boolean): Boolean =
    foldRight(false)((a, b) => predicate(a) && b)

  def forall2(predicate: A => Boolean): Boolean =
    this match {
      case Empty => true
      case StreamCons(head, tail) =>
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
        case Empty               => Empty
        case StreamCons(head, _) => Stream.cons(head(), b)
      }
    )

  def append[B >: A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((a, b) => Stream.cons(a, b))

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)((stream) =>
      stream match {
        case Empty                  => None
        case StreamCons(head, tail) => Some((f(head()), tail()))
      }
    )

  def take2(n: Int): Stream[A] =
    Stream.unfold((0, this))((values) => {
      val (index, stream) = values

      stream match {
        case Empty                  => None
        case _ if index == n        => None
        case StreamCons(head, tail) => Some((head(), (index + 1, tail())))
      }
    })

  def takeWhile3(predicate: A => Boolean): Stream[A] =
    Stream.unfold(this)((stream) =>
      stream match {
        case Empty => None
        case StreamCons(head, tail) => {
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
        case (StreamCons(headA, tailA), StreamCons(headB, tailB)) =>
          Some(((Some(headA()), Some(headB())), (tailA(), tailB())))
        case (StreamCons(headA, tailA), Empty) =>
          Some((Some(headA()), None), (tailA(), Empty))
        case (Empty, StreamCons(headB, tailB)) =>
          Some(((None, Some(headB())), (Empty, tailB())))
      }
    )

  def startsWith[B >: A](stream: Stream[B]): Boolean = {
    @annotation.tailrec
    def go(xs: Stream[A], ys: Stream[B]): Boolean =
      (xs, ys) match {
        case (_, Empty) => true
        case (Empty, _) => false
        case (StreamCons(headA, tailA), StreamCons(headB, tailB)) =>
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
        case Empty => None
        case StreamCons(head, tail) =>
          Some((Stream.cons(head(), tail()), tail()))
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
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    StreamCons(() => head, () => tail)
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
        case (StreamCons(headA, tailA), StreamCons(headB, tailB)) =>
          Some((f(headA(), headB()), (tailA(), tailB())))
      }
    )
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](foldable: F[A]): List[A] =
    foldLeft(foldable)(Nil: List[A])((list, a) => a :: list)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)((a, b) => f(a, b))

  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldRight(z)((a, b) => f(b, a))

  def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
    as.foldRight(m.zero)((a, b) => m.op(b, f(a)))
}

sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  def foldRight[A, B](tree: Tree[A])(z: B)(f: (A, B) => B): B =
    tree match {
      case Leaf => z
      case Node(value, left, right) => {
        val reducedRightValue = foldRight(right)(z)(f)
        foldRight(left)(reducedRightValue)(f)
      }
    }

  def foldLeft[A, B](tree: Tree[A])(z: B)(f: (B, A) => B): B =
    tree match {
      case Leaf => z
      case Node(value, left, right) => {
        val reducedLeftValue = foldLeft(left)(z)(f)
        foldLeft(right)(reducedLeftValue)(f)
      }
    }

  def foldMap[A, B](tree: Tree[A])(f: A => B)(m: Monoid[B]): B =
    tree match {
      case Leaf => m.zero
      case Node(value, left, right) => {
        val reducedLeftValue = foldMap(left)(f)(m)
        val reducedRightValue = foldMap(right)(f)(m)
        m.op(m.op(f(value), reducedLeftValue), reducedRightValue)
      }
    }
}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A, B](option: Option[A])(z: B)(f: (A, B) => B): B =
    option match {
      case None    => z
      case Some(x) => f(x, z)
    }

  def foldLeft[A, B](option: Option[A])(z: B)(f: (B, A) => B): B =
    option match {
      case None    => z
      case Some(x) => f(z, x)
    }

  def foldMap[A, B](option: Option[A])(f: A => B)(m: Monoid[B]): B =
    option match {
      case None    => m.zero
      case Some(x) => f(x)
    }
}

object Main extends App {
  val intAddition = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b

    def zero: Int = 0
  }

  val m = Monoid.productMonoid(intAddition, intAddition)

  println(ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m))

  println(Monoid.bag(Vector("a", "rose", "is", "a", "rose")))
}
