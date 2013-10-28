package org.leetcode

import scala.collection.mutable.ArrayBuffer

/*
 *  Given a collection of numbers, return all possible permutations.
 *  For example,
 *  [1,2,3] have the following permutations:
 *  [1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], and [3,2,1]. 
 * 
 **/
object Permutations {

  def main(args: Array[String]) {
    val result: Array[Array[Int]] = permute(Array(1, 2, 3, 4))
    
    for( i <- 0 until result.length) {
      Console.out.println (result(i).mkString)
    }
  }

  def permute(numbers: Array[Int]): Array[Array[Int]] = {
    var result = ArrayBuffer[Array[Int]]()
    var temp: Array[Int] = numbers clone
    
    for(num <- 0  until temp.length) {
      val subresult: Array[Array[Int]] = permute(temp tail);
      if (subresult.length > 0) {
        for (x <- subresult) {
          result.append(((temp head) +: x))
        }
      } else {
        result append(Array(temp.head))
      }
      
      temp = leftShift(temp)
    }

    result toArray
  }
  
  def leftShift(arr: Array[Int]): Array[Int] = {
      (arr slice (1, arr.length)) :+ arr(0)
  }

}