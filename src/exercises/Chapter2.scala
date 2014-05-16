package exercises

import scala.annotation.tailrec

object Chapter2 {

  /* EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. The
  first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
  the previous two. Your definition should use a local tail-recursive function. */

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) acc
      else loop(n - 1, acc, acc + prev)
    }

    loop(n, 0, 1)
  }

  /* EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
  sorted according to a given comparison function.*/

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(remaining: Array[A], sorted: Boolean): Boolean = {
      if (remaining.length <= 1) sorted
      else loop(remaining.tail, gt(remaining(1), remaining(0)) && sorted)
    }

    loop(as, true)
  }

  /* EXERCISE 3 (hard): Implement partial1 and write down a concrete usage
  of it. */

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  /* EXERCISE 4 (hard): Let's look at another example, currying, which converts a
  function of N arguments into a function of one argument that returns another
  function as its result.
  */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  /* EXERCISE 5 (optional): Implement uncurry, which reverses the
  transformation of curry. */

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  /* EXERCISE 6: Implement the higher-order function that composes two
  functions. */

  def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))

  def main(args: Array[String]) {
    val partial = partial1[Int, Int, Int](5, (a, b) => a + b)
    assert(partial(3) == 8)
    val curried = curry[Int, Int, Int]((a, b) => a + b)
    assert(curried(3)(5) == 8)
    val uncurried = uncurry[Int, Int, Int](curried)
    assert(uncurried(5, 3) == 8)
    val composed = compose[Int, Int, Int](_ + 1, _ + 1)
    assert(composed(3) == 5)
  }
}
