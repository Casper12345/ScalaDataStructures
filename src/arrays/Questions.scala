package arrays

import java.nio.charset.Charset
import scala.annotation.tailrec

object Questions extends App {

  /* Is Unique: Implement an algorithm to determine if a string has all unique characters.
  What if you cannot use additional data structures? */

  def isUnique(s: String): Boolean = {
    @tailrec
    def go(i: Int, m: Map[Char, Int]): Boolean = {
      if(i < s.length) {
        val char = s.charAt(i)
        if(m.contains(char)) go(i + 1, m +((char, m(char) + 1))) else go(i + 1, m +((char, 1)))
      } else {
        m.values.forall(_ < 2)
      }
    }
    go(0, Map.empty)
  }
  // without additional data structures
  def isUnique2(s: String): Boolean = {
    val sorted = s.sortWith(_ < _)
    @tailrec
    def go(i: Int): Boolean = { //
      if(i < sorted.length - 1){
        if(sorted(i) == sorted(i + 1)) false else go(i + 1)
      } else {
        true
      }
    }

    if(sorted.isEmpty) true else go(0)

  }

  // Given two strings, write a method to decide if one is a permutation of the other.

  def isPermutation(s1: String, s2: String): Boolean = {
    def go(p: String, s: String, acc: List[String]): List[String] = {
      if(s.isEmpty) p :: acc else {
        val seq = for {i <- 0 until s.length} yield {
          go(p + s.charAt(i), s.substring(0, i) + s.substring(i+1, s.length), acc)
        }
        seq.foldLeft(List.empty[String])(_ ++ _)
      }
    }
    if(s1.length == s2.length) go("", s1, Nil).contains(s2) else false
  }

  // URLify: Write a method to replace all spaces in a string with '%20:
  // You may assume that the string has sufficient space at the end to hold the additional characters,
  // and that you are given the "true" length of the string. (Note: If implementing in Java,
  // please use a character array so that you can perform this operation in place.)


  def toUrl(s: String, l: Int): String = {
    val toReturn = new Array[Char](s.length)
    @tailrec
    def go(i: Int, e: Int): Unit = {
      if(i < l)  {
        val c = s(i)
        if(c == ' '){
          toReturn(e) = '%'
          toReturn(e + 1) = '2'
          toReturn(e + 2) = '0'
          go(i + 1, e + 3)
        } else {
          toReturn(e) = s(i)
          go(i + 1, e + 1)
        }
      }
    }
    go(0, 0)
    new String(toReturn)
  }
  println(toUrl(" Mr John Smith         ", 15))

}
