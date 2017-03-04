package chapter2

/**
  * Created by KOHEI on 2017/03/04.
  */
object result {
  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(curry((a: Int, b: Int) => a + b)(2)(2))
    println(uncurry(curry((a: String, b: String) => a + "," + b))("hello", "world"))
    println(compose((b: String) => b + "world!!", (a: String) => a + ",")("hello"))
  }

  // 0,1,1,2,3,5,8,13
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(f: Int, s: Int, n: Int): Int = {
      if (n == 0) f else loop(s, f + s, n - 1)
    }

    loop(0, 1, n)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
