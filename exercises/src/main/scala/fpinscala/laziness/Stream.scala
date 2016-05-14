package fpinscala.laziness

import scala.{Stream ⇒ _}
import Stream._

import scala.annotation.tailrec


trait Stream[+A] { self ⇒

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    self match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = self match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) empty
    else self match {
      case Empty ⇒ empty
      case Cons(h, t) ⇒ cons(h(), t().take(n - 1)) // FIX: t is evaluated early here.
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    if (n <= 0) self
    else self match {
      case Empty ⇒ empty
      case Cons(_, t) ⇒ t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty ⇒ empty
    case Cons(h, t) ⇒ {
      val hv = h() // XXX: Have to carefully evaluate this once here...
      if (p(hv)) cons(hv, t().takeWhile(p)) else empty
    }
  }

  def takeWhileFromAnswers(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = foldRight[List[A]](Nil)(_ :: _)
}

// XXX: re-evaluate representation to make it easier to be lazy?
// XXX: It's just that `Cons` should be private.
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}