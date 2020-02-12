package PEG.lexparse

import PEG.data._
import PEG.generators.PEGGenerator

import scala.util.Try



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
