package org.leetcode

/*
 * Given a sorted array, remove the duplicates in place such that each element appear only once and return the new length.
 * Do not allocate extra space for another array, you must do this in place with constant memory.
 * 
 * For example, Given input array A = [1,1,2],
 * Your function should return length = 2, and A is now [1,2]. 
 **/
object RemoveDuplicate {

  def main(args: Array[String]) {
    {
      val test = Array(1, 1, 1, 2, 2, 2, 3, 3, 4)
      val ans = removeDuplicates(test)
      Console.out.print(test.mkString + " => " + (test dropRight(test.length - ans)).mkString)
    }
  }

  def removeDuplicates(A: Array[Int]): Int = {
    var length = A.length

    for (i <- 0 until (length - 1)) {
      var j = i + 1
      while (j < length && A(j) == A(i)) {
        j += 1
      }

      val duplicates = j - i - 1;
      for (k <- 0 until (length - j)) {
        A(j + k - duplicates) = A(j + k);
      }

      length = length - duplicates; //Remove all duplicates.
    }

    length
  }

}