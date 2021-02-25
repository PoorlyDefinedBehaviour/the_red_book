trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
}

object Main extends App {
  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String) = a + b
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]) = a ++ b
    def zero = Nil
  }

  def intAdittionMonoid = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    def zero = 0
  }

  def intMultiplicationMonoid = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    def zero = 1
  }

  def booleanOrMonoid = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a || b
    def zero = false
  }

  def booleanAndMonoid = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)
    def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g

    def zero: A => A = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(b, f(a)))

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) {
      m.zero
    } else if (as.length == 1) {
      f(as(0))
    } else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  println(stringMonoid.op("hello ", "world"))
  println(stringMonoid.op(stringMonoid.zero, "hello world") == "hello world")
  println(listMonoid.op(List(1, 2, 3), List(4, 5, 6)))
  println(intAdittionMonoid.op(2, 2))
  println(intAdittionMonoid.op(intAdittionMonoid.zero, 2) == 2)
  println(intMultiplicationMonoid.op(2, 2))
  println(intMultiplicationMonoid.op(intMultiplicationMonoid.zero, 2) == 2)
  println(booleanOrMonoid.op(true, false))
  println(booleanOrMonoid.op(false, true))
  println(booleanOrMonoid.op(booleanOrMonoid.zero, true))
  println(booleanOrMonoid.op(booleanOrMonoid.zero, false))
  println(booleanAndMonoid.op(true, false))
  println(booleanAndMonoid.op(true, true))
  println(booleanAndMonoid.op(false, true))
  println(booleanAndMonoid.op(booleanAndMonoid.zero, true))
  println(booleanAndMonoid.op(booleanAndMonoid.zero, false))
  println(optionMonoid.op(Some(1), Some(2)))
  println(optionMonoid.op(optionMonoid.zero, Some(2)))
  println(optionMonoid.op(None, Some(2)))
  println(endoMonoid.op(endoMonoid.zero, (x: Int) => x)(3))
  println(endoMonoid[Int].op(_ + 1, _ + 1)(3))
  println(
    List("Hic", "Est", "Index").foldRight(stringMonoid.zero)(stringMonoid.op)
  )
  println(
    List("Hic", "Est", "Index").foldLeft(stringMonoid.zero)(stringMonoid.op)
  )
  println(concatenate(List("hello ", "world"), stringMonoid))
  println(concatenate(List(1, 2, 3), intAdittionMonoid))
  println(foldMap(List("1", "2", "3"), intMultiplicationMonoid)(_.toInt))
  println(foldMap2(List("1", "2", "3"), intMultiplicationMonoid)(_.toInt))
  println(foldMapV(IndexedSeq(1, 2, 3, 4, 5, 6), intAdittionMonoid)(_ * 2))
}
