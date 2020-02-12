package PEG.generators

import PEG.PEGParser.GeneratedPEGParser
import PEG.lexparse.Lexer

import scala.util.{Failure, Success}

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
    """
      | Int <- [0-9]+ { int | xs | flattenNoWS(xs).toInt }
      |
      |""".stripMargin



}
