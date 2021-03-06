package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.annotation.tailrec
import scala.{Stream ⇒ _}

trait Stream[+A] { as ⇒

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    as match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = as match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) empty
    else as match {
      case Empty ⇒ empty
      case Cons(h, t) ⇒ cons(h(), t().take(n - 1)) // FIX: t is evaluated early here.
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    if (n <= 0) as
    else as match {
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

  def takeWhile2(p: A => Boolean): Stream[A] =
    as.foldRight(empty[A]) { (a, b) ⇒
      if (p(a)) cons(a, b) else empty
    }

  def takeWhileFromAnswers(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    as.foldRight(true)((a, b) ⇒ p(a) && b)

  def headOption: Option[A] =
    as.foldRight[Option[A]](None)((a, _) ⇒ Some(a))

  // 5.7 map, filter, append, flatMap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ cons(f(a), b))

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else b)

  def append[AA >: A](o: ⇒ Stream[AA]): Stream[AA] = foldRight(o)(cons(_, _))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    as.foldRight(empty[B])((a, b) ⇒ f(a) append b)

  def startsWith[A](os: Stream[A]): Boolean =
    as.zipAll(os).takeWhile { case (l, r) ⇒ r.isDefined }.forAll {
      case (Some(l), Some(r)) ⇒ l == r
      case  _ => false
    }

  def toList: List[A] = foldRight[List[A]](Nil)(_ :: _)

  // Exercise 5.13. map, take, takeWhile, zipWith in terms of unfold.
  def mapViaUnfold[B](f: A ⇒ B): Stream[B] =
    unfold[B, Stream[A]](as) {
      case Empty ⇒ None
      case Cons(a, as) ⇒ Some((f(a()), as())) // FIX: Evaluates `as` early!
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(as) {
      case Empty ⇒ None
      case Cons(a, as) ⇒ if (n <= 0) None else Some((a(), as())) // FIX: evaluates `as`!
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(as) {
      case Empty ⇒ None
      case Cons(a, as) ⇒ if (p(a())) Some((a(), as())) else None // FIX: evaluates `as`!
    }

  def zipWithViaUnfold[B, C](o: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    unfold((as, o)) {
      case ((Cons(a, as), Cons(b, bs))) ⇒ Some((f(a(), b()), (as(), bs()))) // FIX: early evaluation of tails.
      case _ ⇒ None
    }

  def zip[B](bs: Stream[B]) = as.zipWithViaUnfold(bs) { case (a, b) ⇒ (a, b) }

  def zipAll[B](o: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((as, o)) {
      case (Cons(a, as), Cons(b, bs)) ⇒ Some(Some(a()) → Some(b()), as()  → bs())
      case (Cons(a, as), Empty)       ⇒ Some(Some(a()) → None     , as()  → empty)
      case (Empty,       Cons(b, bs)) ⇒ Some(None      → Some(b()), empty → bs())
      case (Empty,       Empty)       ⇒ None
    }
  }

  def tails: Stream[Stream[A]] =
    unfold(as) {
      case Empty ⇒ None
      case s@Cons(a, as) ⇒ Some(s → as())
    } append cons(empty, empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, ⇒ B) ⇒ B): Stream[B] =
    unfold(as) {
      case Empty ⇒ None
      case s@Cons(a, as) ⇒ Some(s.foldRight(z)(f) → as())
    } append cons(z, empty)
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None ⇒ empty
      case Some((a, s)) ⇒ cons(a, unfold(s)(f))
    }
  }

  def fibs2(): Stream[Int] =
    unfold((0, 1)) { case (a, b) ⇒ Some((a, (b, a + b))) }

  def constant2[A](a: A): Stream[A] = unfold[A, Unit](a)(_ ⇒ Some(a, ()))

  val ones2: Stream[Int] = unfold[Int, Unit](1)(_ ⇒ Some(1, ()))

  def ns() = cons(1, {println("after 1"); cons(2, {println("after 2"); cons(3, empty)})})
}