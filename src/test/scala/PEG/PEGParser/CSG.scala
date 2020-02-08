package PEG.PEGParser

import PEG.ast.PEGAst
import PEG.lexparse.Lexer

import scala.util.{Failure, Success}

object CSG extends ParserGenerator {
  val pack = "PEG.PEGParser"
  val name = "CSGGenerated"
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\test\scala\"""

  val source: String =
    """
      | A <- 'a' A 'b'
      |    /
      |
      | B <- 'b' B 'c'
      |    /
      |
      | D <- &(A !'b') 'a'* B EOF
      |
      | EOF <- !.
      |""".stripMargin

  def getGrammar: Map[String,PEGAst] = {
    val lexer = new Lexer(source)
    //val parser = new BasePEGParser(lexer)
    val parser = new GeneratedPEGParser(lexer)
    parser.Grammar().map(PEGGenerator.toAst) match {
      case Failure(exception) =>
        println(exception)
        throw exception
      case Success(grammer) =>
        grammer
    }
  }

  def main(args: Array[String]): Unit = {
    val grammer = getGrammar
    this.genParserToFile(grammer)
  }
}
