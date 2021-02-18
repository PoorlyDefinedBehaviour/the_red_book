object Main extends App {
  def findFirst[A](xs: Array[A], predicate: A => Boolean): Int = {
    @annotation.tailrec
    def go(index: Int): Int = {
      if (index > xs.length - 1) {
        -1
      } else if (predicate(xs(index))) {
        index
      } else {
        go(index + 1)
      }

    }

    go(0)
  }

  println(findFirst(Array(1, 2, 3), (x: Int) => x == 1))
  println(findFirst(Array(1, 2, 3), (x: Int) => x == 2))
  println(findFirst(Array(1, 2, 3), (x: Int) => x == 3))
  println(findFirst(Array(1, 2, 3), (x: Int) => x == 32))
  println(findFirst(Array(1, 2, 3), (x: Int) => x == -32))
}
