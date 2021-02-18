object Main extends App {
  def abs(n: Int): Int = {
    if (n < 0) {
      -n
    } else {
      n
    }
  }

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

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val message = "The %s of %d is %d."
    message.format(name, n, f(n))
  }

  println(formatResult("absolute value", -42, abs))
  println(formatResult("factorial", 5, tailRecursiveFactorial))
}
