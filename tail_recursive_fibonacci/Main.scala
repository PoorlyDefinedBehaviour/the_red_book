object Main extends App {
  /**
    * time O(n)
    * space O(1)
    */
  def tailRecursiveFibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, a: Int, b: Int): Int = {
      if (i > n - 1) {
        a
      } else {
        go(i + 1, b, a + b)
      }

    }

    go(0, 0, 1)
  }

  println(tailRecursiveFibonacci(0))
  println(tailRecursiveFibonacci(1))
  println(tailRecursiveFibonacci(2))
  println(tailRecursiveFibonacci(3))
  println(tailRecursiveFibonacci(4))
  println(tailRecursiveFibonacci(5))
  println(tailRecursiveFibonacci(6))
}
