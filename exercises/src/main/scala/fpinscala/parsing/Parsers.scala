package fpinscala.parsing

import scala.language.higherKinds
import scala.language.implicitConversions

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  val numA: Parser[Int] = char('a').many.map(_.size)

  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: Parser[B]) = or(p1, p2)

    def many = self.many(p1)

    def map[B](f: A => B) = self.map(p1)(f)
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