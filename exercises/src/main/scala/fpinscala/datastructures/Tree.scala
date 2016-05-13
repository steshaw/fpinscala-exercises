package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) ⇒ value
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(l, r) ⇒ 1 + depth(l) max depth(r)
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
    println(s"maximum(t) = ${maximum(t)}")
    println(s"depth(t) = ${depth(t)}")
  }

}