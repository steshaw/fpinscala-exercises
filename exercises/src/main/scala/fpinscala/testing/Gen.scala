package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

case class Prop(run: (TestCases, RNG) => Result) { p1 ⇒
  def &&(p2: Prop): Prop = Prop {
    (testCases, rng) ⇒ {
      val r1: Result = p1.run(testCases, rng)
      if (r1.isFalsified) r1
      else p2.run(testCases, rng)
    }
  }

  def ||(p2: Prop): Prop = Prop {
    (testCases, rng) ⇒ {
      val r1: Result = p1.run(testCases, rng)
      if (r1.isFalsified) p2.run(testCases, rng)
      else r1
    }
  }

  def label(label: String) = Prop { (testCases, rng) ⇒
    p1.run(testCases, rng) match {
      case f@Falsified(failedCase, _) ⇒ f.copy(s"label: " + failedCase)
      case passed ⇒ passed
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](ga: Gen[A])(p: A ⇒ Boolean): Prop = Prop {
    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    (n, rng) ⇒ randomStream(ga)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) ⇒ try {
        if (p(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception ⇒ Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
}

case class Gen[A](sample: State[RNG, A]) { ga ⇒
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a ⇒ f(a).sample))

  def listOfN(genSize: Gen[Int]): Gen[List[A]] = for {
    size ← genSize
    as ← Gen.listOfN(size, ga)
  } yield as

  def unsized: SGen[A] = SGen { _ ⇒ ga }
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

case class SGen[+A](forSize: Int => Gen[A])
