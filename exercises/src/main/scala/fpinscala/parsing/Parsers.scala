package fpinscala.parsing

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait Parsers[Parser[+_]] { self =>

  // Primitives

  def string(s: String): Parser[String]

  def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def attempt[A](p: Parser[A]): Parser[A] // Allow explicit backtracking ala Parsec.

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def eof: Parser[Unit]

  // Error utils
  def errorMessage(e: ParseError): String

  def errorLocation(e: ParseError): Location

  // Non-primitives

  def succeed[A](a: A): Parser[A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(f.andThen(succeed))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap { r1 =>
      p2.flatMap { r2 =>
        succeed(f(r1, r2))
      }}

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def optional[A](p: Parser[A], default: A) =
    p | succeed(default)

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) ? Nil

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p)) { (r, rs) => r :: rs}

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

    def ?(default: A) = optional(p1, default)

    def many = self.many(p1)

    def map[B](f: A => B) = self.map(p1)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)

    def slice: Parser[String] = self.slice(p1)
  }

  object Laws {
//    import fpinscala.testing._
//
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

//    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//      forAll(inputs ** Gen.string) { case (input, msg) =>
//        run(label(msg)(p))(input) match {
//          case Left(e) => errorMessage(e) == msg
//          case _ => true
//        }
//      }
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
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  // Returns the line corresponding to this location
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(
  stack: List[(Location, String)] = List(),
  otherFailures: List[ParseError] = List()
) {
  def label(errMsg: String): ParseError = {
    val location = stack.head._1
    copy(stack = List((location, errMsg)))
  }
  def scope(errMsg: String): ParseError = {
    val prevStack = stack
    val location = stack.head._1 // FIX: probably wrong
    copy(stack = (location, errMsg) :: prevStack)
  }
}

case class MyParser[+A](f: String => Either[ParseError, (A, String)], errMsg: Option[String] = None)

object MyParsers extends Parsers[MyParser] {

  override def succeed[A](a: A): MyParser[A] = MyParser(input => Right(a, input))

  override def string(s: String): MyParser[String] = MyParser { input =>
    println(s"string s=$s input=$input")
    val r = if (input.startsWith(s)) Right((s, input.substring(s.length)))
    else Left(ParseError(List((Location(input), s"Error matching string '$s'"))))
    println(s"string r=$r")
    r
  }

  override def regex(r: Regex): MyParser[String] = MyParser { input =>
    val maybeMatch: Option[Match] = r.findPrefixMatchOf(input)
    if (maybeMatch.isEmpty) Left(ParseError(List((Location(input), s"Error regex '$r' did not match input '$input'"))))
    else Right((maybeMatch.get.matched, maybeMatch.get.after.toString))
  }

  // XXX: Don't think I can implement slice with my representation...
  // Return the string consumed rather than the value parsed.
  override def slice[A](p: MyParser[A]): MyParser[String] = ???

  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] = MyParser { input =>
    p.f(input).left.map(_.label(msg))
  }

  override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = MyParser { input =>
    p.f(input).left.map(_.scope(msg))
  }

  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = MyParser { input =>
    println(s"MyParsers.flatMap input=$input")
    val r: Either[ParseError, (A, String)] = p.f(input)
    println(s"MyParsers.flatMap r=$r")
    r.right.flatMap { case (a, remainingInput) =>
      println(s"MyParsers.flatMap a=$a remainingInput=$remainingInput")
      val parserB: MyParser[B] = f(a)
      val r1: Either[ParseError, (B, String)] = parserB.f(remainingInput)
      println(s"MyParsers.flatMap r1=$r1")
      r1
    }
  }

  override def attempt[A](p: MyParser[A]): MyParser[A] = ???

  override def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = MyParser { input =>
    val r = p1.f(input)
    if (r.isLeft) p2.f(input) else r
  }

  override def eof: MyParser[Unit] = MyParser { input =>
    if (input.isEmpty) Right((), input)
    else Left(ParseError(List((Location(input), s"Expected eof but got '$input'"))))
  }

  // Error utils
  override def errorMessage(e: ParseError): String = e.stack.head._2

  override def errorLocation(e: ParseError): Location = e.stack.head._1

  // Other
  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
    p.f(input).right.map { case (a, remaining) => a }
}

object JsonParsing {

  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  val jsonNumberRegex = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r

  val jsonStringRegex =
    """" ([^"\\]* | \\["\\bfnrt\/] | \\u[0-9a-fA-F]{4} )* """".filterNot(_.isSpaceChar).r

  val hackJsonString = (s: String) => {
    // XXX: What horrible hacks has our jsonStringRegex caused us to consider?!?

    println(s"s = [[$s]]")
    // Remove double quotes.
    val s1 = s.replaceFirst("^\"", "")
    println(s"s1 = [[$s1]]")
    val s2 = s1.replaceFirst("\"$", "")
    println(s"s2 = [[$s2]]")

    // NOTE: "\\x5C" is "\". This is so that we can match literal "\" as a regex.

    // Replace control escapes.
    val s3 = s2.replaceAll("\\x5C\\x62", "\b") // "\\x62" is "b"
    println(s"s3 = [[$s3]]")
    val s4 = s3.replaceAll("\\x5C\\x66", "\f") // "\\x66" is "f"
    println(s"s4 = [[$s4]]")
    val s5 = s4.replaceAll("\\x5C\\x6E", "\n") // "\\x6E" is "n"
    println(s"s5 = [[$s5]]")
    val s6 = s5.replaceAll("\\x5C\\x72", "\r") // "\\x72" is "r"
    println(s"s6 = [[$s6]]")
    val s7 = s6.replaceAll("\\x5C\\x74", "\t") // "\\x74" is "t"
    println(s"s7 = [[$s7]]")

    // Replace unicode escapes.
    // NOTE: "\\x75" is the same as "u". This is done to avoid being an incorrect unicode matching expression.
    val s8 = "\\x5C\\x75[0-9a-fA-F]{4}".r.replaceAllIn(s7, m => {
      println(s"matched [[${m.matched}]]")
      val s = m.matched.tail.tail // Remove "\\u"
      val codePoint = Integer.parseInt(s, 16)
      val chars = Character.toChars(codePoint)
      new String(chars)
    })
    println(s"s = [[$s8]]")

    s8
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    import JSON._

    // XXX: Do we need slice here?
    // FIX: What's the definition of "spaces" in the spec?
    val spaces = (char(' ') | char('\t') | char('\n')).many //.slice    // XXX: Do we need slice?
    val dquote = char('"')
    val anyChar = """.""".r

    def w[A](p1: Parser[A]): Parser[A] = p1 <* spaces
    def ws[A](p: Parser[A]) = w(p) // XXX: Just alias for calling from MoreParserOps.

    case class MoreParserOps[A](p1: Parser[A]) {
      def w: Parser[A] = ws(p1)

      def *>[B](p2: Parser[B]) = p1 ** p2 map { case (_, r) => r } // Ignore left result.
      def <*[B](p2: Parser[B]) = p1 ** p2 map { case (r, _) => r } // Ignore right result.
    }
    implicit def moreOps[A](p: Parser[A]): MoreParserOps[A] = MoreParserOps[A](p)

    //val jsonString = dquote ** jsonStringBody ** dquote map { case ((_, s), _) => JString(s) }

    val jsonString = (jsonStringRegex map { s => JString(hackJsonString(s)) }).w

    val jsonNumber: Parser[JNumber] = jsonNumberRegex.map((s: String) => JNumber(s.toDouble)).w

    case class JField(name: String, value: JSON)

    def jsonValue = jsonString | jsonNumber | jsonObject | jsonArray | jsonTrue | jsonFalse | jsonNull

    def jsonField = (jsonString <* w(":")) ** jsonValue map { case (name, value) =>
        JField(name.get, value)
    }

    def surround[A, B](lbrace: Parser[A], rbrace: Parser[A])(body: Parser[B]): Parser[B] =
      lbrace *> body <* rbrace

    def sepBy[A, B](sep: Parser[A], body: Parser[B]): Parser[List[B]] = {
      lazy val rest = sep *> sepBy(sep, body)

      (body ** (rest ? Nil) map { case (b, bs) => b :: bs }) ? Nil
    }

    def jsonObject: Parser[JObject] = scope("Parsing JSON Object")(surround(w("{"), w("}"))(sepBy(w(","), jsonField)) map { fields =>
      // XXX: What happens to duplicate names in the map? Here, the last value "wins".
      // XXX: Should we reject them. Spec is unclear.
      JObject(fields.map(f => f.name -> f.value).toMap)
    }).w

    def jsonArray: Parser[JArray] = scope("Parsing JSON Array")(surround(w("["), w("]"))(sepBy(w(","), jsonValue)) map { v =>
      JArray(v.toIndexedSeq)
    }).w

    def jsonTrue = string("true").map(_ => JBool(true)).w
    def jsonFalse = string("false").map(_ => JBool(false)).w
    def jsonNull = string("null").map(_ => JNull).w

    val jsonRoot = scope("Invalid root element")(spaces *> (jsonArray | jsonObject)) <* eof

    jsonRoot
  }

  // For console testing.
  val p = jsonParser(MyParsers)
  val x = (input: String) => MyParsers.run(p)(input)
}
