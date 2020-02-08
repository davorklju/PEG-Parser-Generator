package PEG.PEGParser

import java.io.{File, PrintWriter}
import PEG.ast._
import scala.collection.mutable.ArrayBuffer

trait ParserGenerator {
  val path: String
  val pack: String
  val name: String

  ///////////////////////////////////////////////////////////////////////

  def genParserToFile( grammer: Map[String,PEGAst] ): Unit = {
    val str = pack.replaceAll("\\.", "\\\\")
    val fullPath = s"$path\\$str\\$name.scala"

    val file = new File(fullPath)
    if(!file.exists()){
      file.createNewFile()
    }
    val out = new PrintWriter(file)

    genParser(grammer).foreach{ x =>
      out.println(x)
    }

    out.close()
  }
  ///////////////////////////////////////////////////////////////////////////////////////

  def flatten(p: PTree): String =
    p match {
      case PEmpty => ""
      case PLeaf(node) => node
      case PBranch(_, children) => children.map(flatten).mkString("")
    }

  ///////////////////////////////////////////////////////////////////////////////////////

  private def genParser( grammer: Map[String,PEGAst] ): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]

    buf += s"package $pack"
    buf +=
      """import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
        |import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
        |
        |import scala.collection.mutable.ArrayBuffer
        |import scala.util.{Failure, Try}
        |""".stripMargin

    buf += s"class $name(lexer: Lexer) extends Parser(lexer){"
    for( (rule,ast) <- grammer ) {
      buf += "\n"
      buf ++= genGrammer(rule,ast)
    }
    buf += "}"
    buf
  }

  ///////////////////////////////////////////////////////////////////////

  private var iota: Int = 0

  private def freshVar(prefix: String): String = {
    val i = iota
    iota += 1
    s"${prefix}_$iota"
  }


  private def genGrammer(name: String, ast:PEGAst): ArrayBuffer[String] = {
    var buf = ArrayBuffer.empty[String]

    buf += s"def $name(): Try[PTree] = {"
    buf ++= genAst(name,ast)
    buf += "}"

    buf
  }

  @scala.annotation.tailrec
  private def genAst(name: String, ast: PEGAst): ArrayBuffer[String] =
    ast match {
      case Var(ident) => genVar(ident)
      case Lit(chars) => genLit(chars)
      case Class(chars) => genClass(chars)
      case Any => genAny()
      case Empty => ArrayBuffer( s" Try(PEmpty) " )

      case Cat(asts) => genCat(name,asts)
      case Alt(asts) => genAlt(name,asts)

      case Star(ast) => genStar(name,ast)
      case NegLook(ast) => genNeg(name,ast)

      case Plus(ast) =>
        val newAst = Cat(Seq( ast, Star(ast) ))
        genAst(name,newAst)

      case Optional(ast) =>
        val newAst = Alt(Seq( ast, Empty ))
        genAst(name,newAst)

      case PosLook(ast) =>
        val newAst = NegLook(NegLook(ast))
        genAst(name,newAst)
    }

  private def genVar(ident: String): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]
    val pos = freshVar("pos")

    buf += s"val $pos = mark"
    buf += s"$ident().recoverWith{ case p: ParseError =>"
    buf += s"reset($pos)"
    buf += s""" Failure( p ~ ParseFailed("expected Var '$ident'",$pos) ) """
    buf += "}"

    buf
  }

  private def genLit(chars: Seq[Char]): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]
    val pos = freshVar("pos")
    val res = freshVar("res")

    val cs = chars.map{ x => (freshVar("char_part"),escape(x)) }
    val children = cs.map(_._1).map{ v =>
      s""" PLeaf($v.toString) """
    }.mkString(",")
    val err = chars.map{ escapeDoubleQuote }.mkString(",")

    buf += s"val $pos= mark"
    buf += s"val $res = for{"
    cs.foreach{ case (v,expect) =>
      buf += s"$v <- expect($expect)"
    }
    buf += s""" } yield PBranch("Lit",Seq($children)) """
    buf += s"$res.recoverWith{ case p: ParseError =>"
    buf += s"reset($pos)"
    buf += s""" Failure( p ~ ParseFailed("expected $err",$pos) ) """
    buf += "}"

    buf
  }

  def escape(c: Char): String =
    c match {
      case '\n' => s"'\\n'"
      case '\r' => s"'\\r'"
      case '\t' => s"'\\t'"
      case '\'' => s"'\\''"
      case '\\' => s"'\\\\'"
      case _ => s"'$c'"
    }

  def escapeDoubleQuote(c: Char): String =
    c match {
      case '\"' => """\""""
      case _ => escape(c)
    }

  private def genClass(chars: Set[Char]): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]
    val pos = freshVar("pos")
    val char = freshVar("char")

    val cs = chars.map{ escape }.mkString(",")
    val err = chars.map{ escapeDoubleQuote }.mkString(",")

    buf += s"val $pos = mark"
    buf += s"expect($cs)"
    buf += s".map{ $char => PLeaf($char.toString)}"
    buf += s".recoverWith{ case p: ParseError =>"
    buf += s"reset($pos)"
    buf += s""" Failure( p ~ ParseFailed("Expected one of $err",$pos) ) """
    buf += s"}"
    buf
  }

  private def genAny(): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]
    val pos = freshVar("pos")

    buf += s"val $pos = mark"
    buf += s"any.map{ x => PLeaf(x.toString)  }"
    buf += s".recoverWith{ case p: ParseError =>"
    buf += s"reset($pos)"
    buf += s""" Failure( p ~ ParseFailed("Expected any char",$pos) )  """
    buf += "}"
  }

  private def genCat(name: String, asts: Seq[PEGAst]): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]

    val pos = freshVar("pos0")
    val res = freshVar("res")

    val astsNamed = asts.map{ x => freshVar("catPart") -> x}
    val names = astsNamed.map(_._1).mkString(",")

    buf += s"val $pos = mark"
    buf += s"val $res = for{"
    astsNamed.foreach{ case (name,ast) =>
      ast match {
        case Var(ident) =>
          buf += s"$name <- $ident()"
        case _ =>
          buf += s"$name <- {"
          buf ++= genAst(name,ast)
          buf += "}"
      }
    }
    buf += s""" }  yield PBranch("$name",Seq( $names )) """

    buf += s"$res.recoverWith{ case p: ParseError => "
    buf += s"reset($pos)"
    buf += s"Failure( p  )"
    buf += "}"

    buf
  }

  private def genAlt(name: String, asts: Seq[PEGAst]): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]
    val pos = freshVar("pos")

    def qqq(asts: Seq[PEGAst], errs: List[String]): Unit =
      if(asts.isEmpty) {
        val err0 = s""" ParseFailed("",$pos) """
        val err = errs.foldLeft(err0){ case (acc,p) => s"$p ~ $acc" }
        buf += s"Failure($err)"
      }
      else {
        val x = asts.head
        val res1 = freshVar("res")
        val err1 = freshVar("err")

        buf += s"val $res1 = {"
        buf ++= genAst(name,x)
        buf += "}"

        buf += s"$res1.recoverWith{ case $err1: ParseError =>"
        buf += s"reset($pos)"
        qqq(asts.tail,err1 :: errs)
        buf += "}"
      }

    buf += s"val $pos = mark"
    qqq(asts,List.empty)
    buf
  }

  private def genStar(name: String, ast: PEGAst): ArrayBuffer[String] = {
    val buf = ArrayBuffer.empty[String]

    val parts = freshVar("parts")
    val pos = freshVar("pos")
    val res = freshVar("res")

    buf += s"var $parts = ArrayBuffer.empty[PTree]"
    buf += s"var $pos = mark"
    buf += s"var $res = {"
    buf ++= genAst(name,ast)
    buf += "}"
    buf += s"$res.recover{ _ => reset($pos) }"

    buf += s"while($res.isSuccess){"
    buf += s"$parts += $res.get"

    buf += s"$pos = mark"
    buf += s"$res = {"
    buf ++= genAst(name,ast)
    buf += "}"
    buf += s"$res.recover{ _ => reset($pos) }"

    buf += "}"

    buf += s""" Try(PBranch("$name",$parts.toSeq)) """

    buf
  }

  private def genNeg(name: String, ast: PEGAst): ArrayBuffer[String] = {
    val pos = freshVar("pos")
    val res = freshVar("res")

    val buf = ArrayBuffer.empty[String]

    buf += s"val $pos = mark"
    buf += s"val $res = {"
    buf ++= genAst(name,ast)
    buf += "}"

    buf += s"reset($pos)"

    buf += s"""if( $res.isSuccess ) Failure(ParseFailed("Neglook failed",$pos))"""
    buf += s" else { Try(PEmpty) }"

    buf
  }
}
