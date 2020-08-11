package PEG.generators

import PEG.PEGParser.{GeneratedPEGParser}
import PEG.lexparse.Lexer
import PEG.generators._

import scala.util.{Failure, Success, Try}

object ExprWithAction extends ParserGenerator {
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\main\scala\"""
  val pack = "PEG.generators"
  val name = "ExprWithActionGenerated"

  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Grammar().map{PEGGenerator.toAst}

    result match {
      case Failure(exception) => throw exception
      case Success(grammar) =>
        this.genParserToFile(grammar)
    }
  }

  val source =
      s"""
       |
       | Stmt <- WS Expr EOF {Int| _ e _ | e}
       |
       | Expr <- Fact PLUS Expr {Int| a _ b | a + b }
       |       / Fact MINUS Expr {Int| a _ b | a - b }
       |       / Fact
       |
       | Fact* <- Lit TIMES Fact {Int| a _ b| a * b}
       |       / Lit
       |
       | Lit* <- OPEN Expr CLOSE {Int| _ e _ | e}
       |      / MINUS Lit {Int| _ e| -e}
       |      / Int
       |
       | Int <- [0-9]+ WS { Int | PBranch(_,xs) _ | xs.map(PEGGenerator.flattenNoWS).mkString("").toInt }
       |
       | MINUS <- '-' WS
       | PLUS  <- '+' WS
       | TIMES <- '*' WS
       | OPEN  <- '(' WS
       | CLOSE <- ')' WS
       |
       | WS <- [ \t\n\r]*
       |
       | EOF <- !.
       |
       | ######################################################
       |
       | QQQ <- WS Start EOF { List[Int] | _ x _ | x}
       |
       | Start <- Int Follow { List[Int] | x y | x :: y}
       |
       | Follow <- PLUS Start { List[Int] | _ x | x}
       |         /    { List[Int] | | Nil }
       |
       | ######################################################
       |
       |  IE <- Int { Expr | x | I(x) }
       |
       |  LE <- MINUS EE { Expr | _ x | Neg(x) }
       |      / OPEN EE CLOSE { Expr | _ x _ | x }
       |      / IE
       |
       | FE <- LE TIMES FE {Expr | a _ b| Mul(a,b) }
       |     / LE
       |
       | EE <- FE PLUS EE { Expr | a _ b | Add(a,b) }
       |     / FE
       |
       | """.stripMargin

}

