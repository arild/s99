package s99

import Solutions.???

trait BinaryTreesSolutions {

  sealed abstract class Tree[+T] {
    def addValue[S >: T](s: S): Tree[S] = ???
    def isSymmetric: Boolean = ???
    def preOrder: List[T] = ???
    def inOrder: List[T] = ???
    def toDotString: String = ???
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def leafCount: Int = ???
    def leafList: List[T] = ???
    def internalList: List[T] = ???
    def atLevel(n: Int): List[T] = ???
    def layoutBinaryTree: PositionedNode[T] = ???
    def layoutBinaryTree2: PositionedNode[T] = ???
    def layoutBinaryTree3: PositionedNode[T] = ???

    def show: String = ???
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {

    def cBalanced(n: Int,  s: String): List[Node[String]] = ???
    def fromList(list: List[Int]): Tree[Int] = ???
    def symmetricBalancedTrees(n: Int,  s: String): List[Node[String]] = ???
    def hbalTrees(n: Int,  s: String): List[Node[String]] = ???

    def minHbalNodes(n: Int): Int = ???
    def maxHbalHeight(n: Int): Int = ???
    def hbalTreesWithNodes(n: Int,  s: String): List[Node[String]] = ???
    def completeBinaryTree(n: Int,  s: String): List[Node[String]] = ???

    def fromString(string: String): Node[Char] = ???
    def fromDotString(string: String): Node[Char] = ???

    def string2Tree(string: String): Tree[Char] = ???

    def preInTree(lists: List[Char]*): Node[Char] = ???
  }

  class PositionedNode[+T](override val value: T,
                           override val left: Tree[T],
                           override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" +
      value.toString + " " + left.toString + " " + right.toString + ")"
  }

}
