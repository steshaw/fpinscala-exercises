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

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
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

  val testChoose = listOfN(10000, Gen.choose(0, 3)).
    sample.run(RNG.Simple(0))._1.distinct.sorted == List(0, 1, 2)
}

trait SGen[+A] {
}

