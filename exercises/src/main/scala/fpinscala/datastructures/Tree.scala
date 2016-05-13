package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def id[A](t: Tree[A]): Tree[A] = t match {
    case Leaf(value) ⇒ Leaf(value)
    case Branch(l, r) ⇒ Branch(l, r)
  }

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

  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(v) ⇒ Leaf(f(v))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(leaf: A ⇒ B)(branch: (B, B) ⇒ B): B = t match {
    case Leaf(v) ⇒ leaf(v)
    case Branch(l, r) ⇒ branch(fold(l)(leaf)(branch), fold(r)(leaf)(branch))
  }

  import Function.const

  def id2[A](t: Tree[A]) = fold(t)(v ⇒ Leaf(v): Tree[A])((l, r) ⇒ Branch(l, r))
  def size2[A](t: Tree[A]): Int = fold(t)(const(1))((l, r) ⇒ 1 + l + r)
  def maximum2(t: Tree[Int]) = fold(t)(identity)((l, r) ⇒ l max r)
  def depth2[A](t: Tree[A]) = fold(t)(const(1))((l, r) ⇒ 1 + l max r)
  def map2[A, B](t: Tree[A])(f: A ⇒ B) = fold(t)(v ⇒ Leaf(f(v)): Tree[B])((l, r) ⇒ Branch(l, r))
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
    println(s"id(t) = ${id(t)}")
    println(s"size(t) = ${size(t)}")
    println(s"maximum(t) = ${maximum(t)}")
    println(s"depth(t) = ${depth(t)}")
    println(s"map(t)(_ + 1) = ${map(t)(_ + 1)}")
    println
    println(s"id2(t) = ${id2(t)}")
    println(s"size2(t) = ${size2(t)}")
    println(s"maximum2(t) = ${maximum2(t)}")
    println(s"depth2(t) = ${depth2(t)}")
    println(s"map2(t)(_ + 1) = ${map2(t)(_ + 1)}")
  }

}