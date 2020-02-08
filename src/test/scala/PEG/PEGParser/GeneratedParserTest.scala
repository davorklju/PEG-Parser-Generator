package PEG.PEGParser

import PEG.PEGParser.CSGGenerated
import PEG.PEGParser.Expr.mkInt
import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseFailed}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class GeneratedParserTest extends AnyFlatSpec with Matchers {

  "ExprParser" should "match pos integers" in {
    val source = "125"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    tree.foreach(println)
    Expr.ast2expr(tree.get) shouldBe I(125)
  }

  it should "match singe digit numners" in {
    val source = "1"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    Expr.ast2expr(tree.get) shouldBe I(1)
  }

  it should "match neg integers" in {
    val source = "-125"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    Expr.ast2expr(tree.get) shouldBe Neg(I(125))
  }

  it should "match neg paren" in {
    val source = "-(2 + 3)"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    Expr.ast2expr(tree.get) shouldBe Neg(Add(I(2),I(3)))
  }

  it should "match addition of integers" in {
    val source = "12 + 8"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    Expr.ast2expr(tree.get) shouldBe Add(I(12),I(8))
  }

  it should "match multiplication of integers" in {
    val source = "3 * 4"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isSuccess)
    Expr.ast2expr(tree.get) shouldBe Mul(I(3),I(4))
  }

  it should "multiplication over addition" in {
    val ps = Map(
      "2 + 3 * 4" -> Add(I(2),Mul(I(3),I(4))),
      "2 * 3 + 4" -> Add(Mul(I(2),I(3)),I(4))
    )

    for( (source,e) <- ps ){
      val lexer = new Lexer(source)
      val parser = new PEG.PEGParser.ExprGenerated(lexer)
      val tree = parser.Stmt()
      assert(tree.isSuccess)
      Expr.ast2expr(tree.get) shouldBe e
    }
  }

  it should "brackets over multiplication" in {
    val ps = Map(
      "(2 + 3) * 4" -> Mul(Add(I(2),I(3)),I(4)),
      "2 * (3 + 4)" -> Mul(I(2),Add(I(3),I(4)))
    )

    for( (source,e) <- ps ){
      val lexer = new Lexer(source)
      val parser = new PEG.PEGParser.ExprGenerated(lexer)
      val tree = parser.Stmt()
      assert(tree.isSuccess)
      Expr.ast2expr(tree.get) shouldBe e
    }
  }

  it should "fail to a character" in {
    val source = "asjkds"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isFailure)
  }

  it should "fail fail consecutive +'s" in {
    val source = "2 + + 3"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isFailure)
    println(tree)
  }

  it should "fail fail consecutive ints" in {
    val source = "2  3"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isFailure)
  }

  it should "fail fail unmached brackets" in {
    val source = "( 2 + 3"
    val lexer = new Lexer(source)
    val parser = new PEG.PEGParser.ExprGenerated(lexer)
    val tree = parser.Stmt()
    assert(tree.isFailure)
  }


  "(a^n)(b^n)(c^n)" should "an equal number of a's b's and c's" in {
    val ss = List("","abc","aabbcc","aaabbbccc","aaaabbbbcccc")
    for( source <- ss ){
      val lexer = new Lexer(source)
      val parser = new CSGGenerated(lexer)
      println(source)
      parser.D().map(CSG.flatten) shouldBe Try(source)
    }
  }


  "(a^n)(b^n)(c^n)" should "fail for uneven a's b's and c's" in {
    val ss = List("ab","abbc","aabbc","aabbccc","aaaxbbbbcccc")
    for( source <- ss ){
      val lexer = new Lexer(source)
      val parser = new CSGGenerated(lexer)
      println(source)
      assert(parser.D().isFailure)
    }
  }

  it should "fail for all other characters" in {
    val ss = List("q","asdasd","aabbdd","123","\n\t\r .")
    for( source <- ss ){
      val lexer = new Lexer(source)
      val parser = new CSGGenerated(lexer)
      println(source)
      assert(parser.D().isFailure)
    }
  }

}

