sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf                 => 0
      case Node(_, left, right) => 1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf => 0
      case Node(value, left, right) =>
        value max maximum(left) max maximum(right)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf => 0
      case Node(_, left, right) =>
        1 + depth(left) max 1 + depth(right)
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf => Leaf
      case Node(value, left, right) =>
        Node(f(value), map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A], initialValue: B)(f: (A, B, B) => B): B =
    tree match {
      case Leaf => initialValue
      case Node(value, left, right) =>
        f(value, fold(left, initialValue)(f), fold(right, initialValue)(f))
    }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree, Leaf: Tree[B])((value, left, right) =>
      Node(f(value), left, right)
    )

  def size2[A](tree: Tree[A]): Int =
    fold(tree, 0)((value, leftSize, rightSize) => rightSize + leftSize + 1)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree, 0)((value, leftMaximum, rightMaximum) =>
      value max leftMaximum max rightMaximum
    )

  def depth2[A](tree: Tree[A]): Int =
    fold(tree, 0)((_value, leftDepth, rightDepth) =>
      leftDepth + 1 max rightDepth + 1
    )
}

object Main extends App {
  println(Tree.size(Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))))
  println(Tree.size(Leaf))
  println(Tree.size(Node(1, Leaf, Leaf)))
  println(Tree.maximum(Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))))
  println(
    Tree.depth(
      Node(
        1,
        Node(2, Node(3, Leaf, Node(5, Leaf, Leaf)), Leaf),
        Node(4, Leaf, Leaf)
      )
    )
  )
  println(
    Tree.map(
      Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
    )(_ * 2)
  )
  println(
    Tree.map2(
      Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
    )(_ * 2)
  )
  println(Tree.size2(Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))))
  println(Tree.size2(Leaf))
  println(Tree.size2(Node(1, Leaf, Leaf)))
  println(Tree.maximum2(Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))))
  println(
    Tree.depth2(
      Node(
        1,
        Node(2, Node(3, Leaf, Node(5, Leaf, Leaf)), Leaf),
        Node(4, Leaf, Leaf)
      )
    )
  )
}
