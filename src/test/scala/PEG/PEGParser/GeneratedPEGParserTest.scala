package PEG.PEGParser

import PEG.data._
import PEG.generators.PEGGenerator
import PEG.lexparse.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class GeneratedPEGParserTest extends AnyFlatSpec with Matchers {

  "EndOfFile" should "return Success(()) when called on am empty string" in {
    val source = ""
    val parser = mkParser(source)

    assert(parser.EndOfFile().isSuccess)
  }

  it should "return Failure(...) when called on a non empty string" in {
    val source = "aaa"
    val parser = mkParser(source)

    parser.EndOfFile() shouldBe Symbol("isFailure")
  }

  it should "consume 0 characters on a success" in {
    val source = ""
    val parser = mkParser(source)

    parser.EndOfFile()
    parser.mark shouldBe 0
  }

  "EndOfLine" should " return Success(()) for \\n \\r \\r\\n" in {
    for (str <- List("\n", "\r", "\r\n")) {
      val parser = mkParser(str)
      assert(parser.EndOfLine().isSuccess )
    }
  }

  it should " return Success(()) and consume 2 characters when called with \\r\\n" in {
    val source = "\r\n"
    val parser = mkParser(source)
    parser.EndOfLine()
    parser.mark shouldBe 2
  }

  "Space" should "support ' ' \\t \\n \\r \\r\\n" in {
    val source = " \t\n\r\r\n"
    val parser = mkParser(source)
    for(_ <- 0.until(5)) {
      assert(parser.Space().isSuccess)
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "consume 1 char on ' ' \\t \\n \\r" in {
    val source = " \t\n\r"
    val parser = mkParser(source)
    for {_ <- source} {
      val start = parser.mark
      parser.Space()
      (parser.mark - start) shouldBe 1
    }
  }

  it should "consume 2 char on ' ' \\r\\n" in {
    val source = "\r\n"
    val parser = mkParser(source)
    parser.Space()
    parser.mark shouldBe 2
  }

  it should "fail on empty input" in {
    val source = ""
    val parser = mkParser(source)
    assert(parser.Space().isFailure)
    assert(parser.EndOfFile().isSuccess)
  }

  "Comment" should "work with a new line" in {
    val source =
      """#hello world
        |""".stripMargin
    val parser = mkParser(source)
    assert(parser.Comment().isSuccess)
  }

  it should "consume 14 characters for '#hello world\r\n'" in {
    val source =
      """#hello world
        |""".stripMargin
    val parser = mkParser(source)
    parser.Comment()
    parser.mark shouldBe 14
  }

  it should "consume 13 characters for '#hello world\n'" in {
    val source = "#hello world\n"
    val parser = mkParser(source)
    parser.Comment()
    parser.mark shouldBe 13
  }

  it should "fail on empty input" in {
    val source = ""
    val parser = mkParser(source)
    assert(parser.Comment().isFailure)
  }

  "Spaces" should "support all whitespace character ' ' \\t \\n \\r \\r\\n" in {
    val source = " \t\n\r\r\n"
    val parser = mkParser(source)
    assert(parser.Spacing().isSuccess)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support comments " in {
    val source =
      """# Hello world
        |""".stripMargin
    val parser = mkParser(source)
    assert(parser.Spacing().isSuccess)
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
    val parser = mkParser(source)
    assert(parser.Spacing().isSuccess)
    assert(parser.EndOfFile().isSuccess)
  }

  "SingeCharThenWhitespace" should "match each individual character" in {
    for (str <- List(".", ".   ")) {
      val parser = mkParser(str)
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
      val parser = mkParser(str)
      assert(parser.LEFTARROW().isSuccess)
      assert(parser.EndOfFile().isSuccess)
    }
  }

  "Char" should "match regular characters" in {
    val source = "abc123_+"
    val parser = mkParser(source)
    for (c <- source) {
      parser.Char().map{PEGGenerator.flattenChars} shouldBe Try(List(c))
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "match escape characters" in {
    val source = "\\n\\t\\[\\]\\\\"
    val parser = mkParser(source)
    for (c <- List('\n', '\t', '[', ']', '\\')) {
      parser.Char().map(PEGGenerator.flattenChars) shouldBe Try(List(c))
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on unsupported escape characters" in {
    val source = "\\s"
    val parser = mkParser(source)
    assert(parser.Char().isFailure)
  }

  it should "support a combination of regular and support characters" in {
    val source = "abc123\\n\\t\\["
    val parser = mkParser(source)
    for (c <- List('a', 'b', 'c', '1', '2', '3', '\n', '\t', '[')) {
      parser.Char().map(PEGGenerator.flattenChars) shouldBe Try(List(c))
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on empty input" in {

    val source = ""
    val parser = mkParser(source)
    assert(parser.Char().isFailure)
  }

  "Range" should "support singe characters" in {
    val source = "abc"
    val parser = mkParser(source)
    for (c <- source) {
      val charClass: Try[List[Char]] = parser.Range().map(PEGGenerator.flattenChars)
      charClass shouldBe Try(List(c))
    }
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support ranges" in {
    val source = "a-c2-3"
    val parser = mkParser(source)
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(('a' to 'c').toList)
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(('2' to '3').toList)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "support mix of characters and ranges" in {
    val source = "ab0-5cd7-9"
    val parser = mkParser(source)
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(List('a'))
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(List('b'))
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(('0' to '5').toList)
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(List('c'))
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(List('d'))
    parser.Range().map(PEGGenerator.flattenChars) shouldBe Try(('7' to '9').toList)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "fail on empty input" in {
    val source = ""
    val parser = mkParser(source)
    assert(parser.Range().isFailure)
  }


  "Class" should "parse empty class" in {
    val source = "[]"
    val parser = mkParser(source)
    parser.Class().map(PEGGenerator.tree2ast) shouldBe Try(Class(Set.empty))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse escape characters" in {
    val source = "[\\n\\t\\[\\]]"
    val parser = mkParser(source)
    parser.Class().map(PEGGenerator.tree2ast) shouldBe Try( Class( "\t\n[]".toSet ) )
    assert( parser.EndOfFile().isSuccess )
  }

  it should "parse singe char classes" in {
    val source = "[abc123]"
    val parser = mkParser(source)
    parser.Class().map(PEGGenerator.tree2ast) shouldBe Try(Class(Set('a', 'b', 'c', '1', '2', '3')))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse range classes" in {
    val source = "[a-z0-9]"
    val parser = mkParser(source)
    val expected = 'a'.to('z').toSet union '0'.to('9').toSet
    parser.Class().map(PEGGenerator.tree2ast) shouldBe Try(Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "parse a mix of characters and ranges" in {
    val source = "[+_()a-z0-9]"
    val parser = mkParser(source)
    val expected =
      'a'.to('z').toSet union
        '0'.to('9').toSet union
        "+_()".toSet
    parser.Class().map(PEGGenerator.tree2ast) shouldBe Try(Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be followed by spaces" in {
    val source = "[]    "
    val parser = mkParser(source)
    parser.Class()
    assert(parser.EndOfFile().isSuccess)
  }

  "Literal" should "allow empty string" in {
    for (q <- List("'", "\"", "`")) {
      val source = s"$q$q"
      val parser = mkParser(source)
      parser.Literal().map(PEGGenerator.tree2ast) shouldBe Try(Lit(List.empty))
    }
  }

  it should "allow anything else" in {
    val cases = Map(
      "'abc'" -> List('a', 'b', 'c'),
      "`\\t`" -> List('\t'),
      "\"[0-9]\"" -> List('[', '0', '-', '9', ']')
    )
    for ((source, expected) <- cases) {
      println(source)
      val parser = mkParser(source)
      parser.Literal().map(PEGGenerator.tree2ast) shouldBe Try(Lit(expected))
      assert(parser.EndOfFile().isSuccess)
      println("----")
    }
  }

  "Ident" should "not be empty" in {
    val source = ""
    val parser = mkParser(source)
    assert(parser.Identifier().isFailure)
  }

  it should "be length 1" in {
    val source = "A"
    val parser = mkParser(source)
    parser.Identifier().map(PEGGenerator.tree2ast) shouldBe Try(Var("A"))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be length any length > 1" in {
    val source = "_12Abc"
    val parser = mkParser(source)
    parser.Identifier().map(PEGGenerator.tree2ast) shouldBe Try(Var(source))
    assert(parser.EndOfFile().isSuccess)
  }

  "Primany" should "be a dot `.`" in {
    val source = "."
    val parser = mkParser(source)
    parser.Primary().map(PEGGenerator.tree2ast) shouldBe Try(PEG.data.Any)
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a char class" in {
    val source = "[+/_()0-9\\]]"
    val parser = mkParser(source)
    val expected = "+/_()]".toSet union '0'.to('9').toSet
    parser.Primary().map(PEGGenerator.tree2ast) shouldBe Try(PEG.data.Class(expected))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a literal" in {
    val token = "hello world"
    for (p <- "'`\"".toList) {
      val parser = mkParser(s"$p$token$p")
      parser.Primary().map(PEGGenerator.tree2ast) shouldBe Try(Lit(token.toSeq))
      assert(parser.EndOfFile().isSuccess)
    }
  }

  it should "be a Ident" in {
    val source = "Expr"
    val parser = mkParser(source)
    parser.Primary().map(PEGGenerator.tree2ast) shouldBe Try(Var(source))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "be a paren expr" in {
    val source = "( `aaa` ) "
    val parser = mkParser(source)
    val triedAst: Try[PEGAst] =
      parser.Primary().map(PEGGenerator.tree2ast)
    triedAst shouldBe Try(Lit("aaa".toList))
    assert( parser.EndOfFile().isSuccess )
  }

  it should "fail if LEFTARROW follow Ident" in {
    val source = "Expr <-"
    val parser = mkParser(source)
    assert(parser.Primary().isFailure)
  }

  it should "fail if STAR LEFTARROW follow Ident" in {
    val source = "Expr* <-"
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
      parser.Suffix().map{PEGGenerator.tree2ast} shouldBe Try(expected)
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
      parser.Suffix().map(PEGGenerator.tree2ast) shouldBe Try(expected)
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
      parser.Suffix().map(PEGGenerator.tree2ast) shouldBe Try(expected)
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
      parser.Prefix().map(PEGGenerator.tree2ast) shouldBe Try(expected)
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
      parser.Prefix().map(PEGGenerator.tree2ast) shouldBe Try(expected)
      assert( parser.EndOfFile().isSuccess )
    }
  }

  "Sequence" should "be Empty on empty input" in {
    val parser = mkParser("    ")
    parser.Sequence().map(PEGGenerator.tree2ast) shouldBe Try(Empty)
    parser.Spacing()
    assert(parser.EndOfFile().isSuccess)
  }

  it should "not have a Seq wrapper for singe element seqs" in {
    val source = "Expr"
    val parser = mkParser(source)
    parser.Sequence().map(PEGGenerator.tree2ast) shouldBe Try(Var(source))
  }

  it should "support multiple expressions" in {
    val source = "Fact `+` Expr  "
    val parser = mkParser(source)
    val expected = Cat(Seq(Var("Fact"), Lit(List('+')), Var("Expr") ))
    parser.Sequence().map(PEGGenerator.tree2ast) shouldBe Try(expected)
  }

  "Expression" should "Empty on empty string" in {
    val parser = mkParser("    ")
    parser.Expression().map(PEGGenerator.tree2ast) shouldBe Try(PEG.data.Empty)
  }

  it should "have no alt wrap for 1 alt" in {
    val source = "Expr   "
    val parser = mkParser(source)
    parser.Expression().map(PEGGenerator.tree2ast) shouldBe Try(Var("Expr"))
    assert(parser.EndOfFile().isSuccess)
  }

  it should "have alt wrap for > 2" in {
    val source =
      """A1 A2 / B1 B2 /
        |""".stripMargin
    val parser = mkParser(source)
    val expected =
      Alt( List(
        Cat(List(Var("A1"),Var("A2"))),
        Cat(List(Var("B1"),Var("B2"))),
        PEG.data.Empty
      ) )

    parser.Expression().map(PEGGenerator.tree2ast) shouldBe Try(expected)
    assert(parser.EndOfFile().isSuccess)
  }
/////////////////////////////////////////////////////////////////////
  "Action" should "be between { and } and have 2 seperators |" in {
    val source = "{int|x y| x + y}"
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Action().map{PEGGenerator.flattenAction}
    result match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value shouldBe Option( ("int",List("x","y"),"x + y") )
    }
  }

  it should "support wild and crazy types" in {
    val typeL = "Either[List[Int],Try[Set[Boolean]]]"

    val source = s"{ $typeL | p | q } "
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Action().map{PEGGenerator.flattenAction}
    result match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value shouldBe Option(( typeL, List("p"), "q "))
    }
  }

  it should "support compplex arguments" in {
    val source = "{ qqq | a(b(c),d(e)) f g | ppp}"
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Action().map{PEGGenerator.flattenAction}
    result match {
      case Failure(exception) => throw exception
      case Success(value) =>
       value shouldBe Option(("qqq",List("a(b(c),d(e))","f","g"),"ppp"))
    }
  }

  it should "support compplex arguments with spaces" in {
    val source = "{ qqq | a(b( c ), d(e )) f g | ppp}"
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Action().map{PEGGenerator.flattenAction}
    result match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value shouldBe Option(("qqq",List("a(b(c),d(e))","f","g"),"ppp"))
    }
  }

  "ExprPart" should "match a Sequence with no Action" in {
    val mp = Map(
      "x" -> Var("x"),
      "'hello'" -> Lit("hello".toSeq),
      "[a-z]?" -> Optional(Class('a'.to('z').toSet)),
      "&(!P+)" -> PosLook(NegLook(Plus(Var("P")))),
      "x `hello` &(!P+) [a-z]?" -> Cat(List(
            Var("x"),
            Lit("hello".toSeq),
            PosLook(NegLook(Plus(Var("P")))),
            Optional(Class('a'.to('z').toSet))
          ))
    )
    for( (source,expected) <- mp ){
      val lexer = new Lexer(source)
      val parser = new GeneratedPEGParser(lexer)
      val result = parser.ExprPart().map{PEGGenerator.tree2ast}
      result match {
        case Failure(exception) => throw exception
        case Success(value) =>
          value shouldBe expected
      }
    }
  }

  it should "match a Sequence with an Action" in {
    val mp = Map(
      "x {p | q | r}" -> Action(Var("x"),"p",List("q"),"r"),
      "'hello' {p||}" -> Action(Lit("hello".toList),"p",Nil,""),
      "[a-z]? {QQQ|asd|123}" -> Action(Optional(Class('a'.to('z').toSet))
                    ,"QQQ",List("asd"),"123"),
      "&(!P+) {  z  ||}" -> Action(PosLook(NegLook(Plus(Var("P"))))
                    , "z",Nil,""),
      "x `hello` &(!P+) [a-z]? {string| x h p a| concat(x,h,p,a) }" -> Action(Cat(List(
        Var("x"),
        Lit("hello".toSeq),
        PosLook(NegLook(Plus(Var("P")))),
        Optional(Class('a'.to('z').toSet))
      )), "string",List("x","h","p","a"),"concat(x,h,p,a) ")
    )
    for( (source,expected) <- mp ){
      val lexer = new Lexer(source)
      val parser = new GeneratedPEGParser(lexer)
      val result = parser.ExprPart().map{PEGGenerator.tree2ast}
      result match {
        case Failure(exception) => throw exception
        case Success(value) =>
          value shouldBe expected
      }
    }
  }
  /////////////////////////////////////////////////////////////////////
  "Definition" should " work " in {
    val source =
      """Expr <- Expr `+` Expr
        |      / Int
        |""".stripMargin

    val expected =
      Definition("Expr",false,
        Alt(List(
          Cat(List(Var("Expr"),Lit(List('+')),Var("Expr"))),
          Var("Int")
        ))
      )

    val parser = mkParser(source)

    parser.Definition().map(PEGGenerator.tree2Def) shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess )
  }

  it should " work with empty alt" in {
    val source =
      """Expr <- Expr `+` Expr # comment
        |      / Int
        |      /
        |""".stripMargin

    val expected =
      Definition("Expr",false,
        Alt(List(
          Cat(List(Var("Expr"),Lit(List('+')),Var("Expr"))),
          Var("Int"),
          Empty
        ))
      )

    val parser = mkParser(source)

    parser.Definition().map(PEGGenerator.tree2Def) shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess )
  }

  it should "set memo to true one E* <- ..." in {
    val source = "E* <- F"
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Definition().map{x =>
      PEGGenerator.tree2Def(x)
    }

    result shouldBe Try( Definition("E",true,Var("F")) )
    assert( parser.EndOfFile().isSuccess )
  }

  "Grammar" should "support 1 defintion" in {
    val source =
      """
        | E <- F `+` E # addition
        |    / F
        |
        |""".stripMargin

    val expected = List(
      Definition("E",false,
        Alt(List(
          Cat(List(Var("F"),Lit(List('+')),Var("E"))),
          Var("F")
        ))
      )
    )

    val parser = mkParser(source)
    parser.Grammar().map(PEGGenerator.tree2grammar) shouldBe Try(expected)
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

    val expected = List(
      Definition("E",false,
        Alt(List(
          Cat(List(Var("F"),Lit(List('+')),Var("E"))),
          Var("F")
        ))
      ) ,
      Definition("F",false,
        Alt(List(
          Cat(List(Var("I"),Lit(List('*')),Var("F"))),
          Var("I")
        ))
      ) ,
      Definition("I",false,
        Plus(Class('0'.to('9').toSet))
      )
    )

    val parser = mkParser(source)
    parser.Grammar().map(PEGGenerator.tree2grammar) shouldBe Try(expected)
    assert( parser.EndOfFile().isSuccess)
  }


  it should "support actions on every toplevel alt" in {
    val source =
      """
        | E <- F `+` E {int|f e| f + e} # addition
        |    / F
        |
        | F <- I `*` F {int | i f | i * f } #multiplication
        |    / I
        |
        | I <- [0-9]+ {int| str | flattenNoWS(str).toInt } # an int
        |
        | # akjsdlkjasldjlasjk
        |
        |""".stripMargin

    val expected = List(
      Definition("E",false,
        Alt(List(
          Action( Cat(List(Var("F"),Lit(List('+')),Var("E")))
                ,"int",List("f","e"),"f + e"),
          Var("F")
        ))
      ) ,
      Definition("F",false,
        Alt(List(
          Action( Cat(List(Var("I"),Lit(List('*')),Var("F")))
                ,"int", List("i","f"),"i * f " ),
          Var("I")
        ))
      ) ,
      Definition("I",false,
        Action( Plus(Class('0'.to('9').toSet))
          , "int",List("str"),"flattenNoWS(str).toInt ")
      )
    )

    val parser = mkParser(source)
    parser.Grammar().map(PEGGenerator.tree2grammar) shouldBe Try(expected)
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
    assert( parser.Grammar().isSuccess )
    assert( parser.EndOfFile().isSuccess )
  }

  private def mkParser(source: String) = {
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    parser
  }
}

