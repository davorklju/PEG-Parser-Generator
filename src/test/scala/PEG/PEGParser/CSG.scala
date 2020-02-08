package PEG.PEGParser

import PEG.ast.{PBranch, PEGAst, PEmpty, PLeaf, PTree}
import PEG.lexparse.Lexer

import scala.util.{Failure, Success}

object CSG extends ParserGenerator {
  val pack = "PEG.PEGParser"
  val name = "CSGGenerated"
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\test\scala\"""

  val source =
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

  def getGrammer(): Map[String,PEGAst] = {
    val lexer = new Lexer(source)
    //val parser = new BasePEGParser(lexer)
    val parser = new GeneratedPEGParser(lexer)
    parser.Grammer().map(PEGGenerator.toAst) match {
      case Failure(exception) =>
        println(exception)
        throw exception
      case Success(grammer) =>
        grammer
    }
  }

  def main(args: Array[String]): Unit = {
    val grammer = getGrammer()
    this.genParserToFile(grammer)
  }
}
