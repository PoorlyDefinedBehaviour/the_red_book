object Main extends App {
  /**
    * time O(n)
    * space O(1)
    */
  def tailRecursiveFactorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) {
        acc
      } else {
        go(n - 1, n * acc)
      }
    }

    go(n, 1)
  }

  println(tailRecursiveFactorial(5))
}
