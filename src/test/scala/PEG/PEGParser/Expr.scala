package PEG.PEGParser

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.Lexer


sealed trait Expr
case class I(int: Int) extends Expr
case class Neg(e: Expr) extends Expr
case class Add(l: Expr,r: Expr) extends Expr
case class Mul(l: Expr,r: Expr) extends Expr

///////////////////////////////////////////////////////////

object Expr extends ParserGenerator {
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\test\scala\"""
  val pack = "PEG.PEGParser"
  val name = "ExprGenerated"

  def ast2expr(tree: PTree): Expr =
    tree match {
      case PEmpty => throw new Error("Unexpected PEmpty")
      case PLeaf(_) => throw new Error("Unexpected PLeaf")
      case PBranch(node, children) =>
        node match {
          case "Stmt" => children match {
            case List(_, e, _) => ast2expr(e)
          }
          case "Expr" => children match {
            case List(l,PBranch("PLUS", _),r) =>
              Add(ast2expr(l),ast2expr(r))
          }
          case "Fact" => children match {
            case List(l,PBranch("PROD", _),r) =>
              Mul(ast2expr(l),ast2expr(r))
          }
          case "Term" => children match {
            case List(PEmpty, e) => ast2expr(e)
            case List(_, e) => Neg(ast2expr(e))
          }
          case "Lit" => children match {
            case List(PBranch("OPEN", _),e,PBranch("CLOSE", _)) =>
              ast2expr(e)
          }
          case "Int" => children match {
            case List(PBranch(_,i),_) =>
              I( mkInt(i) )
          }
        }
    }

  def mkInt(value: Seq[PTree]): Int =
    value.map(flatten).mkString("").toInt


  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(this.source)
    val parser = new GeneratedPEGParser(lexer)
    val g = parser.Grammar().map(PEGGenerator.toAst)
    if(g.isFailure) println("parse failed")
    g.foreach(this.genParserToFile)
    println("done")
  }

    val source: String =
      """
        | Stmt <- WS Expr EOF
        |
        | Expr <- Fact PLUS Expr
        |       / Fact
        |
        | Fact <- Term PROD Fact
        |       / Term
        |
        | Term <- '-'? Lit
        |
        | Lit <- OPEN Expr CLOSE
        |      / Int
        |
        | Int <- [0-9]+ WS
        |
        | PLUS <- '+' WS
        | PROD <- '*' WS
        | OPEN <- '(' WS
        | CLOSE <- ')' WS
        | WS <- [ \t\n\r]*
        |
        | EOF <- !.
        |
        |""".stripMargin



}

