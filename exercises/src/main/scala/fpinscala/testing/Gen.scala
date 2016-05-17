package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors, Future}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) { p1 ⇒
  def &&(p2: Prop): Prop = Prop {
    (maxSize, testCases, rng) ⇒ {
      val r1: Result = p1.run(maxSize, testCases, rng)
      if (r1.isFalsified) r1
      else p2.run(maxSize, testCases, rng)
    }
  }

  def ||(p2: Prop): Prop = Prop {
    (maxSize, testCases, rng) ⇒ {
      val r1: Result = p1.run(maxSize, testCases, rng)
      if (r1.isFalsified) p2.run(maxSize, testCases, rng)
      else r1
    }
  }

  def label(label: String) = Prop { (maxSize, testCases, rng) ⇒
    p1.run(maxSize, testCases, rng) match {
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
case object Proved extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](sa: SGen[A])(p: A => Boolean): Prop =
    forAll(sa.forSize(_))(p)

  def forAll[A](fga: Int ⇒ Gen[A])(p: A ⇒ Boolean): Prop = Prop {
    (maxSize, n, rng) ⇒
      val casesPerSize = (n + (maxSize - 1)) / maxSize
      val props: Stream[Prop] = Stream.from(0).take((n min maxSize) + 1).map { i ⇒
        forAll(fga(i))(p)
      }
      val prop: Prop = props.map(p ⇒ Prop { (max, _, rng) ⇒
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(maxSize, n, rng)
  }

  def forAll[A](ga: Gen[A])(p: A ⇒ Boolean): Prop = Prop {
    def randomStream(g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

    def buildMsg(s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    // XXX: Just ignore maxSize here...
    (_, n, rng) ⇒ randomStream(ga)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) ⇒ try {
        if (p(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception ⇒ Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a =>
      val b: Boolean = f(a)(s).get
      // FIX: side effect. Plus, cannot do this here as it's too early?
      //s.shutdown() // Otherwise we leave a bunch of executors running.
      b
    }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def run(
    name: String,
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg ($name)")
      case Passed =>
        println(s"+ OK, passed $testCases tests. ($name)")
      case Proved =>
        println(s"+ OK, proved property. ($name)")
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) { ga ⇒
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) ⇒ C) =
    ga.flatMap(a ⇒ gb.map(b ⇒ f(a, b)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a ⇒ f(a).sample))

  def **[B](gb: Gen[B]): Gen[(A,B)] = (ga map2 gb)((_,_))

  def listOfN(genSize: Gen[Int]): Gen[List[A]] = for {
    size ← genSize
    as ← Gen.listOfN(size, ga)
  } yield as

  def unsized: SGen[A] = SGen { _ ⇒ ga }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a) )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(g))

  def sequence[S, A](fs: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequence(fs.map(_.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n ⇒ start + n % (stopExclusive - start)))

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

case class SGen[+A](forSize: Int => Gen[A]) { sa ⇒
  def map[B](f: A ⇒ B): SGen[B] = SGen(n ⇒ sa.forSize(n).map(f))

  def flatMap[B](f: A ⇒ SGen[B]): SGen[B] = SGen { size ⇒
    sa.forSize(size).flatMap(a ⇒ f(a).forSize(size))
  }
}

object SGen {
  def listOf[A](ga: Gen[A]): SGen[List[A]] = SGen { size ⇒
    Gen.listOfN(size, ga)
  }

  // XXX: nonEmptyListOf
  def listOf1[A](ga: Gen[A]): SGen[List[A]] = SGen { size ⇒
    Gen.listOfN(1 max size, ga)
  }
}

object Examples {
  import SGen._

  val smallInt = Gen.choose(-10, 10)
  val listMaxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val listSortedProp = forAll(listOf(smallInt)) { ns ⇒
    val sorted = ns.sorted
    sorted.length == ns.length &&
      ns.forall(a ⇒ ns.count(_ == a) == sorted.count(_ == a)) &&
      (ns.isEmpty || sorted.zip(sorted.tail).forall {
        case (a, b) ⇒ a <= b // adjacent pairs are sorted.
      })
  }

  def p2(ES: ExecutorService) = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def p3(ES: ExecutorService) = check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val p4 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint: Gen[Par[Int]] = Gen.choose(0,10) map Par.unit

  val pint2: Gen[Par[Int]] = {
    val i1: Gen[Par[Int]] = Gen(State(RNG.int)) map (i ⇒ Par.fork(Par.unit(i)))
    val i2: Gen[Par[Int]] = Gen(State(RNG.int)) map (i ⇒ Par.fork(Par.unit(i)))
    val r: Gen[Par[Int]] = i1.map2(i2)((pi1, pi2) ⇒ Par.map2(pi1, pi2)(_ + _))
    r
  }

  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i))

  def go() = {
    val ES: ExecutorService = Executors.newCachedThreadPool
    try {
      run("listMax", listMaxProp)
      run("listSorted", listSortedProp)
      run("p2", p2(ES))
      run("p3", p3(ES))
      run("p4", p4)
      run("fork", forkProp)
    } catch { case _: Throwable ⇒ ES.shutdown() }
  }
}
