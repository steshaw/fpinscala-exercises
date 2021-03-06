package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { self =>
  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A,B,C,D](
    fa: F[A], fb: F[B], fc: F[C]) (f: (A, B, C) => D
  ): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A,B,C,D,E](
    fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil: List[B]))((a, mbs) => map2(f(a), mbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(fa, replicateM(n - 1, fa))(_ :: _)

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def product[G[_]](G: Applicative[G]): Applicative[λ[x ⇒ (F[x], G[x])]] = {
    val F = self
    new Applicative[λ[x ⇒ (F[x], G[x])]] {
      override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        val fv = F.map2(fa._1, fb._1)(f)
        val gv = G.map2(fa._2, fb._2)(f)
        (fv, gv)
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[λ[x => F[G[x]]]] = {
    val F = self
    new Applicative[λ[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }
    }
  }

  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map(): Map[K, V])) {
      case ((k, fv), b) => map2(fv, b)((v, b) => b + (k -> v))
    }
  }
}

trait Monad[F[_]] extends Applicative[F] { self =>
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def compose[G[_]](G: Monad[G]): Monad[λ[x => F[G[x]]]] = {
    val F = self
    new Monad[λ[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      // Because the type constructors, F & G, alternate, there's no way
      // to join with either F.join or G.join.
      override def join[A](mma: F[G[F[G[A]]]]): F[G[A]] = {
        ???
      }
    }
  }
}

object Monad {
  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
      OptionT(M.flatMap(value) {
        case None ⇒ M.unit(None)
        case Some(a) ⇒ f(a).value
      })
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  // XXX: complete.
//  def optionMonad2[A]: Monad[Option] = OptionT[Id, A]

  def eitherMonad[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.fold(Left(_), f)

    override def unit[A](a: => A): Either[E, A] = Right(a)
  }

  def stateMonad[S] = new Monad[State[S, ?]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[λ[x => F[N[x]]]] =
    new Monad[λ[x => F[N[x]]]] {
      override def unit[A](a: ⇒ A): F[N[A]] = F.unit(N.unit(a))

      override def join[A](fnfna: F[N[F[N[A]]]]): F[N[A]] = {
        val map1: F[N[N[A]]] = F.flatMap(fnfna)(nfna ⇒ T.sequence(nfna))
        val fna: F[N[A]] = F.map(map1)(nna ⇒ N.join(nna))
        fna
      }
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[Validation[E, ?]] = new Applicative[Validation[E, ?]] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (f@Failure(_, _), Success(_)) => f
        case (Success(_), f@Failure(_, _)) => f
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (t1 :+ h2) ++ t2)
      }
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[Const[M, ?]] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  def monoidToApplicative[M](M: Monoid[M]): Applicative[Const[M, ?]] =
    new Applicative[Const[M, ?]] {
      override def unit[A](a: => A): M = M.zero

      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] =
        M.op(fa, fb)

      override def apply[A, B](fab: Const[M, (A) => B])(fa: Const[M, A]): Const[M, B] =
        M.op(fab, fa)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { F ⇒

  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[Const[B, ?],A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, ?],A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    // XXX: Ugly
    val reversed: List[A] = mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2
    val tuple: (F[A], List[A]) = mapAccum(fa, reversed)((_, s) ⇒ (s.head, s.tail))
    tuple._1
  }

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    val tuple: (F[Unit], B) = mapAccum(fa, z)((a, s) ⇒ ((), f(s, a)))
    tuple._2
  }

  def fuseSlow[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                             (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    // NOTE: Works but traverses twice. See fuse.
    val traverse1: G[F[B]] = traverse(fa)(f)
    val traverse2: H[F[B]] = traverse(fa)(g)
    (traverse1, traverse2)
  }

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val applicative: Applicative[λ[x => (G[x], H[x])]] = G.product(H)
    traverse[λ[x => (G[x], H[x])], A, B](fa)(a ⇒ (f(a), g(a)))
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[λ[x => F[G[x]]]] = new Traverse[λ[x => F[G[x]]]] {
    override def traverse[M[_], A, B](fga: F[G[A]])(f: A ⇒ M[B])
                                     (implicit GA: Applicative[M]): M[F[G[B]]] = {
      F.traverse(fga)((ga: G[A]) ⇒ G.traverse(ga)(f))
    }
  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])
                                     (implicit G: Applicative[G]): G[List[B]] = {
      as.foldRight(G.unit(List.empty[B]))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])
                                     (implicit G: Applicative[G]): G[Option[B]] = {
      fa.fold(G.unit(Option.empty[B]))(a => G.map(f(a))(Some(_)))
    }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])
                                     (implicit G: Applicative[G]): G[Tree[B]] = {
      val h_gb: G[B] = f(fa.head)
      val map1: List[G[Tree[B]]] = fa.tail.map(t => traverse(t)(f))
      val sequence1: G[List[Tree[B]]] = G.sequence(map1)
      G.map2(h_gb, sequence1)((b: B, tail: List[Tree[B]]) => Tree(b, tail))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

/*
  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
*/
}

object ApplicativeExample {
  import java.util.Date

  case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthDate: String): Validation[String, Date] =
    try {
      Success(new java.text.SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
    } catch {
      case _: java.text.ParseException => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else
      Failure("Phone number must be 10 digits")

  def validWebForm(name: String, birthDate: String,
                   phone: String): Validation[String, WebForm] = {
    Applicative.validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthDate),
      validPhone(phone))(
      WebForm(_,_,_))
  }
}
