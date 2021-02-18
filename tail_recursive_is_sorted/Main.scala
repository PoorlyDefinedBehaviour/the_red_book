object Main extends App {
  def isSorted[A](xs: Array[A], predicate: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int): Boolean = {
      if (index > xs.length - 2) {
        true
      } else if (predicate(xs(index), xs(index + 1))) {
        go(index + 1)
      } else {
        false
      }
    }

    go(0)
  }

  println(isSorted(Array(), (a: Int, b: Int) => a < b))
  println(isSorted(Array(1), (a: Int, b: Int) => a < b))
  println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
  println(isSorted(Array(1, 3, 2), (a: Int, b: Int) => a < b))
  println(isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b))
}
