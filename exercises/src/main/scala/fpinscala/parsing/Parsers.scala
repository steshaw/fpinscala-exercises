package fpinscala.parsing

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  // Primitives
  def string(s: String): Parser[String]

  def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a) // XXX: actually not primitive

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]


  // Non-primitives

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(f.andThen(succeed))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap { r1 =>
      p2.flatMap { r2 =>
        succeed(f(r1, r2))
      }}

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap { r1 =>
      p2.flatMap { r2 =>
        succeed((r1, r2))
      }}

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p)){case (r, rs) => r :: rs} | succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p ** many(p) map { case (r, rs) => r :: rs}

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))((r, rs) => r :: rs)

  val numA: Parser[Int] = char('a').many.map(_.size)

  // Implicits

  implicit val stringToParser = string _

  implicit val regexToParser = regex _

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: => Parser[B]) = or(p1, p2)

    def **[B](p2: => Parser[B]) = product(p1, p2)

    def many = self.many(p1)

    def map[B](f: A => B) = self.map(p1)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)
  }

  object Laws {
    import fpinscala.testing._

//    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//      forAll(in)(s => run(p1)(s) == run(p2)(s))
//
//    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//      equal(p, p.map(a => a))(in)
//
//    def charProp(c: Gen[Char]): Prop =
//      run(char(c))(c.toString) == Right(c)
//
//    def stringProp(s: Gen[String]): Prop =
//      run(string(s))(s) == Right(s)

    // Product is associative.
    // def productAssociativeProp = (a ** b) ** c == a ** (b ** c)
  }

  trait Usage {
    // Exercise 9.6. -- that many 'a's.
    val thatManyAs = {
      val digit = """[0-9]""".r
      digit.flatMap { digit =>
        val n = digit.toInt
        listOfN(n, char('a'))
      }
    }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
