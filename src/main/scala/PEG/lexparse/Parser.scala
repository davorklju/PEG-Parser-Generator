package PEG.lexparse

import scala.util.Try

sealed trait ParseError[+t] extends Exception

object ParseError{

  object implicits{
    implicit class ParseErrorImpl[t](val self: ParseError[t]){
      def ~(rhs: ParseError[t]): ParseError[t] = (self,rhs) match {
        case (ParseFailed(m1,p1),ParseFailed(m2,p2)) =>
          ParseFailed(s"$m1 at $p1\n$m2",p2).asInstanceOf[ParseError[t]]
        case (_,p@ParseFailed(_,_)) => p
        case (p@ParseFailed(_,_),_) => p
        case (_,p) => p
      }
    }
  }

}
case class UnexpectedEOF(pos: Int) extends ParseError[Nothing]
case class ExpectedOneOf[t](chars: List[t], pos: Int) extends ParseError[t]
case class NotExpectedOneOf[t](chars: List[t], pos: Int) extends ParseError[t]
case class ParseFailed(reason: String, pos: Int) extends ParseError[Nothing]

class Parser(val lexer: Lexer) {
  def mark: Int =
    lexer.mark

  def reset(pos: Int): Unit =
    lexer.reset(pos)

  def lookAhead: Try[Char] =
    if(!lexer.hasMore) Try( throw UnexpectedEOF(mark) )
    else Try{ lexer.peek }

  def any: Try[Char] =
    lookAhead.map{ c =>
      lexer.consume
      c
    }

  def expect(c0: Char, cs: Char*): Try[Char] =
    lookAhead.flatMap{ c =>
      val chars = c0 :: cs.toList
      if( chars contains c ) {
        lexer.consume
        Try( c )
      }
      else Try( throw ExpectedOneOf(chars,mark) )
    }
}
