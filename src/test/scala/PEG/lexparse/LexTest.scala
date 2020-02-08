package PEG.lexparse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LexTest extends AnyFlatSpec with Matchers {
  "A Lexer" should " increment after consume is called " in {
    val source = "abc"
    val lexer = new Lexer(source)
    lexer.mark should be (0)
    lexer.consume
    lexer.mark should be (1)
  }

  it should " change the result of mark after reset " in {
    val source = "xyz"
    val lexer = new Lexer(source)
    lexer.mark should be (0)
    lexer.reset(10)
    lexer.mark should be (10)
  }
}
