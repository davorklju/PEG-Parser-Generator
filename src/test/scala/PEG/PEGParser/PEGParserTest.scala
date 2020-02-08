package PEG.PEGParser

import PEG.PEGParser.BasePEGParser
import PEG.ast.{Alt, Cat, Class, Empty, Lit, NegLook, Optional, Plus, PosLook, Star, Var}
import PEG.lexparse.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class PEGParserTest extends AnyFlatSpec with Matchers {

  "EndOfFile" should "return Success(()) when called on am empty string" in {
    val source = ""
    val parser = mkParser(source)

    parser.EndOfFile() should be(Try(()))
  }

  it should "return Failure(...) when called on a non empty string" in {
    val source = "aaa"
    val parser: BasePEGParser = mkParser(source)

    parser.EndOfFile() shouldBe Symbol("isFailure")
  }

  it should "consume 0 characters on a success" in {
    val source = ""
    val parser: BasePEGParser = mkParser(source)

    parser.EndOfFile()
    parser.mark shouldBe 0
  }

  "EndOfLine" should " return Success(()) for \\n \\r \\r\\n" in {
    for (str <- List("\n", "\r", "\r\n")) {
      val parser: BasePEGParser = mkParser(str)
      parser.EndOfLine() shouldBe Try(())
    }
  }

  it should " return Success(()) and consume 2 characters when called with \\r\\n" in {
    val source = "\r\n"
    val parser: BasePEGParser = mkParser(source)
    parser.EndOfLine()
    parser.mark shouldBe 2
  }

  "Space" should "support ' ' \\t \\n \\r \\r\\n" in {
    val source = " \t\n\r\r\n"
    val parser: BasePEGParser = mkParser(source)
    for(c <- 0.until(5)) {
      assert(parser.Space().isSuccess)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "consume 1 char on ' ' \\t \\n \\r" in {
    val source = " \t\n\r"
    val parser: BasePEGParser = mkParser(source)
    for {c <- source} {
      val start = parser.mark
      parser.Space()
      (parser.mark - start) shouldBe 1
    }
  }

  it should "consume 2 char on ' ' \\r\\n" in {
    val source = "\r\n"
    val parser: BasePEGParser = mkParser(source)
    parser.Space()
    parser.mark shouldBe 2
  }

  it should "fail on empty input" in {
    val source = ""
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Space().isFailure)
    assert(parser.EndOfFile().isSuccess)
  }

  "Comment" should "work with a new line" in {
    val source =
      """#hello world
        |""".stripMargin
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Comment().isSuccess)
  }

  it should "consume 14 characters for '#hello world\r\n'" in {
    val source =
      """#hello world
        |""".stripMargin
    val parser: BasePEGParser = mkParser(source)
    parser.Comment()
    parser.mark shouldBe 14
  }

  it should "consume 13 characters for '#hello world\n'" in {
    val source = "#hello world\n"
    val parser: BasePEGParser = mkParser(source)
    parser.Comment()
    parser.mark shouldBe 13
  }

  it should "fail on empty input" in {
    val source = ""
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Comment().isFailure)
  }

  "Spaces" should "support all whitespace character ' ' \\t \\n \\r \\r\\n" in {
    val source = " \t\n\r\r\n"
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Spaces().isSuccess)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support comments " in {
    val source =
      """# Hello world
        |""".stripMargin
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Spaces().isSuccess)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support a mix of whitespace and comments" in {
    val source =
      """
        |# Hello world
        |
        |
        |# goodbye world
        |
        |""".stripMargin
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Spaces().isSuccess)
    assert(parser.EndOfFile().isSuccess)
  }

  "SingeCharThenWhitespace" should "match each individual character" in {
    for (str <- List(".", ".   ")) {
      val parser: BasePEGParser = mkParser(str)
      assert(parser.DOT().isSuccess)
      assert(parser.EndOfFile().isSuccess)
    }
  }

  it should "fail on any other character" in {
    val source = "A"
    val parser = mkParser(source)
    assert(parser.DOT().isFailure)
  }

  "LEFTARROW" should "match a left arrow" in {
    for (str <- List("<-", "<-   ")) {
      val parser: BasePEGParser = mkParser(str)
      assert(parser.LEFTARROW().isSuccess)
      assert(parser.EndOfFile().isSuccess)
    }
  }

  "Char" should "match regular characters" in {
    val source = "abc123_+"
    val parser: BasePEGParser = mkParser(source)
    for (c <- source) {
      parser.Char() shouldBe Try(c)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "match escape characters" in {
    val source = "\\n\\t\\[\\]\\\\"
    val parser: BasePEGParser = mkParser(source)
    for (c <- List('\n', '\t', '[', ']', '\\')) {
      parser.Char() shouldBe Try(c)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on unsupported escape characters" in {
    val source = "\\s"
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Char().isFailure)
  }

  it should "support a combination of regular and support characters" in {
    val source = "abc123\\n\\t\\["
    val parser: BasePEGParser = mkParser(source)
    for (c <- List('a', 'b', 'c', '1', '2', '3', '\n', '\t', '[')) {
      parser.Char() shouldBe Try(c)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on empty input" in {

    val source = ""
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Char().isFailure)
  }

  "Range" should "support singe characters" in {
    val source = "abc"
    val parser: BasePEGParser = mkParser(source)
    for (c <- source) {
      parser.Range() shouldBe Try(List(c))
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support ranges" in {
    val source = "a-c2-3"
    val parser: BasePEGParser = mkParser(source)
    for (c <- List('a'.to('c').toList, '2'.to('3').toList)) {
      parser.Range() shouldBe Try(c)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support mix of characters and ranges" in {
    val source = "ab0-5cd7-9"
    val parser: BasePEGParser = mkParser(source)
    val value1 =
      List(
        List('a'), List('b'),
        '0'.to('5').toList,
        List('c'), List('d'),
        '7'.to('9').toList
      )
    for (c <- value1) {
      parser.Range() shouldBe Try(c)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on empty input" in {
    val source = ""
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Range().isFailure)
  }


  "Class" should "parse empty class" in {
    val source = "[]"
    val parser: BasePEGParser = mkParser(source)
    parser.Class() shouldBe Try(Class(Set.empty))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse escape characters" in {
    val source = "[\\n\\t\\[\\]]"
    val parser = mkParser(source)
    parser.Class() shouldBe Try(
      Class( "\t\n[]".toSet ))
    assert( parser.EndOfFile().isSuccess )
  }

  it should "parse singe char classes" in {
    val source = "[abc123]"
    val parser: BasePEGParser = mkParser(source)
    parser.Class() shouldBe Try(Class(Set('a', 'b', 'c', '1', '2', '3')))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse range classes" in {
    val source = "[a-z0-9]"
    val parser: BasePEGParser = mkParser(source)
    val expected = 'a'.to('z').toSet union '0'.to('9').toSet
    parser.Class() shouldBe Try(Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse a mix of characters and ranges" in {
    val source = "[+_()a-z0-9]"
    val parser: BasePEGParser = mkParser(source)
    val expected =
      'a'.to('z').toSet union
        '0'.to('9').toSet union
        "+_()".toSet
    parser.Class() shouldBe Try(Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be followed by spaces" in {
    val source = "[]    "
    val parser: BasePEGParser = mkParser(source)
    parser.Class()
    assert(parser.EndOfFile().isSuccess)
  }

  "Literal" should "allow empty string" in {
    for (q <- List("'", "\"", "`")) {
      val source = s"$q$q"
      val parser: BasePEGParser = mkParser(source)
      parser.Literal() shouldBe Try(Lit(List.empty))
    }
  }

  it should "allow anything else" in {
    val cases = Map(
      "'abc'" -> List('a', 'b', 'c'),
      "`\\t`" -> List('\t'),
      "\"[0-9]\"" -> List('[', '0', '-', '9', ']')
    )
    for ((source, expected) <- cases) {
      val parser: BasePEGParser = mkParser(source)
      parser.Literal() shouldBe Try(Lit(expected))
      assert(parser.EndOfFile().isSuccess)
    }
  }

  "Ident" should "not be empty" in {
    val source = ""
    val parser: BasePEGParser = mkParser(source)
    assert(parser.Ident().isFailure)
  }

  it should "be length 1" in {
    val source = "A"
    val parser: BasePEGParser = mkParser(source)
    parser.Ident() shouldBe Try(Var("A"))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be length any length > 1" in {
    val source = "_12Abc"
    val parser: BasePEGParser = mkParser(source)
    parser.Ident() shouldBe Try(Var(source))
    assert(parser.EndOfFile().isSuccess)
  }

  "Primany" should "be a dot `.`" in {
    val source = "."
    val parser: BasePEGParser = mkParser(source)
    parser.Primary() shouldBe Try(PEG.ast.Any)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a char class" in {
    val source = "[+/_()0-9\\]]"
    val parser = mkParser(source)
    val expected = "+/_()]".toSet union '0'.to('9').toSet
    parser.Primary() shouldBe Try(PEG.ast.Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a literal" in {
    val token = "hello world"
    for (p <- "'`\"".toList) {
      val parser = mkParser(s"$p$token$p")
      parser.Primary() shouldBe Try(Lit(token.toSeq))
      assert(parser.EndOfFile().isSuccess)
    }
  }

  it should "be a Ident" in {
    val source = "Expr"
    val parser = mkParser(source)
    parser.Primary() shouldBe Try(Var(source))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a paren expr" in {
    val source = "( `aaa` ) "
    val parser = mkParser(source)
    parser.Primary() shouldBe Try(Lit("aaa".toList))
    assert( parser.EndOfFile().isSuccess )
  }

  it should "fail if LEFTARROW follow Ident" in {
    val source = "Expr <-"
    val parser = mkParser(source)
    assert(parser.Primary().isFailure)
  }

  "Suffix" should "anything followed by a ?" in {
    val ps = Map(
      "Expr ? " -> Optional(Var("Expr")),
      "`cat` ? " -> Optional(Lit("cat".toList)),
      "( Expr ? ) ? " -> Optional(Optional(Var("Expr"))),
      "[abc] ? " -> Optional(Class("abc".toSet))
    )
    for( (src,expected) <- ps ){
      val parser = mkParser(src)
      parser.Suffix() shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }


  it should "anything followed by a *" in {
    val ps = Map(
      "Expr * " -> Star(Var("Expr")),
      "`cat` * " -> Star(Lit("cat".toList)),
      "( Expr * ) * " -> Star(Star(Var("Expr"))),
      "[abc] * " -> Star(Class("abc".toSet))
    )
    for( (src,expected) <- ps ){
      val parser = mkParser(src)
      parser.Suffix() shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }

  it should "anything followed by a +" in {
    val ps = Map(
      "Expr + " -> Plus(Var("Expr")),
      "`cat` + " -> Plus(Lit("cat".toList)),
      "( Expr + ) + " -> Plus(Plus(Var("Expr"))),
      "[abc] + " -> Plus(Class("abc".toSet))
    )
    for( (src,expected) <- ps ){
      val parser = mkParser(src)
      parser.Suffix() shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }

  "Prefix" should "be & then anything" in {
    val ps = Map(
      "& Expr ? " -> PosLook(Optional(Var("Expr"))),
      "& ( [abc] + ) " -> PosLook(Plus(Class("abc".toSet))),
      "& ( & `qqq` * ) " -> PosLook(PosLook(Star(Lit("qqq".toList))))
    )
    for((src,expected) <- ps){
      val parser = mkParser(src)
      parser.Prefix() shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }

  it should "be ! then anything" in {
    val ps = Map(
      "! Expr ? " -> NegLook(Optional(Var("Expr"))),
      "! ( [abc] + ) " -> NegLook(Plus(Class("abc".toSet))),
      "! ( & `qqq` * ) " -> NegLook(PosLook(Star(Lit("qqq".toList))))
    )
    for((src,expected) <- ps){
      val parser = mkParser(src)
      parser.Prefix() shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }

  "Sequence" should "be Empty on empty input" in {
    val parser = mkParser("    ")
    parser.Sequence() shouldBe Try(Empty)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "not have a Seq wrapper for singe element seqs" in {
    val source = "Expr"
    val parser = mkParser(source)
    parser.Sequence() shouldBe Try(Var(source))
  }

  it should "support multiple expressions" in {
    val source = "Fact `+` Expr  "
    val parser = mkParser(source)
    val expected = Cat(Seq(Var("Fact"), Lit(List('+')), Var("Expr") ))
    parser.Sequence() shouldBe Try(expected)
  }

  "Expression" should "Empty on empty string" in {
    val parser = mkParser("    ")
    parser.Expression() shouldBe Try(PEG.ast.Empty)
  }

  it should "have no alt wrap for 1 alt" in {
    val source = "Expr   "
    val parser = mkParser(source)
    parser.Expression() shouldBe Try(Var("Expr"))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "have alt wrap for > 2" in {
    val source =
      """A1 A2
        |/ B1 B2
        |/
        |""".stripMargin
    val parser = mkParser(source)
    val expected =
      Alt( List(
        Cat(List(Var("A1"),Var("A2"))),
        Cat(List(Var("B1"),Var("B2"))),
        PEG.ast.Empty
      ) )

    parser.Expression() shouldBe Try(expected)
    assert(parser.EndOfFile().isSuccess)
  }

  "Definition" should " work " in {
    val source =
      """Expr <- Expr `+` Expr
        |      / Int
        |""".stripMargin

    val expected =
      "Expr" -> Alt(List(
          Cat(List(Var("Expr"),Lit(List('+')),Var("Expr"))),
          Var("Int")
      ))

    val parser = mkParser(source)

    parser.Definition() shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess )
  }

  it should " work with empty alt" in {
    val source =
      """Expr <- Expr `+` Expr # comment
        |      / Int
        |      /
        |""".stripMargin

    val expected =
      "Expr" -> Alt(List(
        Cat(List(Var("Expr"),Lit(List('+')),Var("Expr"))),
        Var("Int"),
        Empty
      ))

    val parser = mkParser(source)

    parser.Definition() shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess )
  }

  "Grammer" should "support 1 defintion" in {
    val source =
      """
        | E <- F `+` E # addition
        |    / F
        |
        |""".stripMargin

    val expected = Map(
      "E" -> Alt(List(
        Cat(List(Var("F"),Lit(List('+')),Var("E"))),
        Var("F")
      ))
    )

    val parser = mkParser(source)
    parser.Grammer() shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess)
  }

  it should "support > 2 definitions" in {
    val source =
      """
        | E <- F `+` E # addition
        |    / F
        |
        | F <- I `*` F
        |    / I
        |
        | I <- [0-9]+ # an int
        |
        | # akjsdlkjasldjlasjk
        |
        |""".stripMargin

    val expected = Map(
      "E" -> Alt(List(
        Cat(List(Var("F"),Lit(List('+')),Var("E"))),
        Var("F")
      )),
      "F" -> Alt(List(
        Cat(List(Var("I"),Lit(List('*')),Var("F"))),
        Var("I")
      )),
      "I" -> Plus(Class('0'.to('9').toSet))
    )

    val parser = mkParser(source)
    parser.Grammer() shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess)
  }


  it should "support (a^n)(b^n)(c^n)" in {
    val source =
      """
        | A <- 'a' A 'b' /
        | B <- 'b' B 'c' /
        | D <- &(A !'b') B
        |""".stripMargin

    val parser = mkParser(source)
    assert( parser.Grammer().isSuccess )
    assert( parser.EndOfFile().isSuccess )
  }

  private def mkParser(source: String) = {
    val lexer = new Lexer(source)
    val parser = new BasePEGParser(lexer)
    parser
  }
}

