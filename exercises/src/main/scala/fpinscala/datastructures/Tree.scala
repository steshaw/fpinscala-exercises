package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

}

object Trees {
  import Tree._

  //
  //                      |
  //              -----------------
  //              |               |
  //              1         ---------------
  //                        |             |
  //                  ------------        4
  //                  |          |
  //                  2          3
  //
  val t = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))

  def test() = {
    println(s"size(t) = ${size(t)}")
  }

}