package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

trait Prop { p1 ⇒
  def check: Boolean

  def &&(p2: Prop): Prop = new Prop {
    def check = p1.check && p2.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) { ga ⇒
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a ⇒ f(a).sample))

  def listOfN(genSize: Gen[Int]): Gen[List[A]] = for {
    size ← genSize
    as ← Gen.listOfN(size, ga)
  } yield as
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a) )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(g))

  def sequence[S, A](fs: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequence(fs.map(_.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state: State[RNG, Int] = State(RNG.nonNegativeLessThan(stopExclusive + start))
    val gen: Gen[Int] = Gen(state)
    gen.map(_ + start)
  }

  def boolean: Gen[Boolean] = choose(0, 2).map(_ == 1)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b ⇒ if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val ratio = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double)).flatMap(d ⇒ {
      if (d < ratio) g1._1 else g2._1
    })
  }

  val testChoose = listOfN(10000, Gen.choose(0, 3)).
    sample.run(RNG.Simple(0))._1.distinct.sorted == List(0, 1, 2)
}

trait SGen[+A] {
}

