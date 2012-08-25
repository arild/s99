package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = list.last
  def penultimate[T](list: List[T]): T = list.takeRight(2).head
  def nth[T](n: Int, list: List[T]): T = list.drop(n).head
  def length[T](list: List[T]): Int = list.foldLeft(0)((r, c) => r + 1)
  def reverse[T](list: List[T]): List[T] = list.foldLeft(List[T]())((r, c) => c :: r)
  def isPalindrome[T](list: List[T]): Boolean = list.take(list.length / 2) == list.reverse.take(list.length / 2)
  def flatten(list: List[Any]): List[Int] = {
    def _flatten[Any](x: Any): List[Int] = {
      x match {
        case l: List[_] => flatten(l)
        case e: Int => List[Int](e)
      }
    }
    list.foldLeft(List[Int]())((r, c) => r ::: _flatten(c))
  }
  def compress[T](list: List[T]): List[T] = list.foldLeft(List[T]())((r, c) => if (r.length == 0 || r.head != c) c :: r else r) reverse

  def pack[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List[List[T]]()
    else {
      val (packed, unpacked) = list.span(_ == list.head)
      packed :: pack(unpacked)
    }
  }
  def encode[T](list: List[T]): List[(Int, T)] = {
    val temp = pack(list)
    temp.map(50)
  }
  def encodeModified[T](list: List[T]): List[Any] = ???
  def decode[T](list: List[(Int, T)]): List[T] = ???
  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???
  def duplicate[T](list: List[T]): List[T] = ???
  def duplicateN[T](n: Int, list: List[T]): List[T] = ???
  def drop[T](n: Int, list: List[T]): List[T] = ???
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???
  def rotate[T](n: Int, list: List[T]): List[T] = ???
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

