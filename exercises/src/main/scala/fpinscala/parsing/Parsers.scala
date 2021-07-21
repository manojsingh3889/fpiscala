package fpinscala.parsing

import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop}

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C]
  def map2WithProduct[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = {
    map(product(p, p2))(a => f(a._1, a._2))
  }
  def productWithMap2[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] =
    map2(p, p2)((a, b) => (a, b))
//  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = {
//    map(p)(a => map(p2)(b => (a, b)))
//  }
  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]
  def many[A](p: Parser[A]): Parser[List[A]]
  //aaa
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a, b) => a :: b)
  }
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
//    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
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