package org.project.euler

import scala.annotation.tailrec

/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5,
 * we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * Find the sum of all the multiples of 3 or 5 below 1000.
 **/
object SumOfMultiples {

  def main(args: Array[String]) {
    def sumOfMultiple35 = sumOfMultiple(Array(3, 5, 7), _: Int);
    Console.out.println("sumOfMultiple =>"+sumOfMultiple35(10));
  }

  def sumOfMultiple(args: Array[Int], upperBound: Int): Int = {
    @tailrec
    def cal(index: Int, acc: Int): Int = {
      if (index >= upperBound) {
        acc
      } else {
        val temp = if (isMultiple(index)) index else 0;
        cal(index + 1, acc + temp)
      }
    }

    def isMultiple(index: Int): Boolean = {
      args.exists(p => (index % p == 0))
    }

    cal(1, 0)
  }

}