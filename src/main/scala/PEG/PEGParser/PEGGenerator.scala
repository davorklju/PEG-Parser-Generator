package PEG.PEGParser

import PEG.ast.{Alt, Any, Cat, Class, Empty, Lit, NegLook, Optional, PBranch, PEGAst, PEmpty, PLeaf, PTree, Plus, PosLook, Star, Var}
import PEG.lexparse.Lexer

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object PEGGenerator extends ParserGenerator {
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\main\scala\"""
  override val pack: String = "PEG.PEGParser"
  override val name: String = "GeneratedPEGParser"

  def grammer2ast(grammer: PTree): Map[String, PEGAst] =
    grammer match {
      case PEmpty => throw new Error()
      case PLeaf(node) => throw new Error()

      case PBranch("Grammer", List(_, PBranch(_, List(x0, rest)), _)) =>
        val xs = rest match {
          case PBranch(_, Nil) => List.empty
          case PBranch(_, xs) => xs
        }
        (x0 :: xs.toList).map(def2ast).toMap
    }

  def def2ast(definition: PTree): (String, PEGAst) =
    definition match {
      case PEmpty => throw new Error()
      case PLeaf(node) => throw new Error()
      case PBranch("Definition", List(ident,_,expression)) =>
          val id = flattenNoWS(ident)
          val expr = tree2ast(expression)
          (id -> expr)
      }

  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)

    val g = parser.Grammer().map(toAst)


    if(g.isFailure) println("parse failed")

    g.foreach{ grammer =>
      println("parse complete")
      this.genParserToFile(grammer)
      println("gen complete")
    }

  }

  def tree2ast(tree: PTree): PEGAst =
    tree match {
      case PEmpty => throw new Error()
      case PLeaf(node) => throw new Error()

      case PBranch("Expression", List(e0, PBranch(_,ls))) =>
        val es = ls.toList.map {
          case PBranch(_,List (PBranch("SLASH", _), e)) => e
        }
        if(es.isEmpty) tree2ast(e0)
        else Alt((e0 :: es).map(tree2ast))

      case PBranch("Sequence",ss) => ss match {
        case Nil => PEG.ast.Empty
        case x :: Nil => tree2ast(x)
        case _  => Cat(ss.map(tree2ast))
      }

      case PBranch("Prefix",List(p,suf)) =>
        val ast = tree2ast(suf)
        p match {
          case PEmpty => ast
          case PBranch("AND", _) => PosLook(ast)
          case PBranch("NOT", _) => NegLook(ast)
        }

      case PBranch("Suffix", List(prim,s)) =>
        val ast = tree2ast(prim)
        s match {
          case PEmpty => ast
          case PBranch("QUESTION",_) => Optional(ast)
          case PBranch("STAR",_) => Star(ast)
          case PBranch("PLUS",_) => Plus(ast)
        }

      case PBranch("Primary", children) => children match{
        case List(id,_) =>
          tree2ast(id)

        case List(PBranch("OPEN",_),e,PBranch("CLOSE",_)) =>
          tree2ast(e)
      }

      case PBranch("Identifier", id) =>
        val str = id.map(flattenNoWS).mkString("")
        Var(str)

      case PBranch("Literal",List(_,lit,_,_)) =>
        Lit(flattenChars(lit))

      case PBranch("DOT",_) =>
        Any

      case PBranch("Class",List(_,cs,_,_)) =>
        Class(flattenChars(cs).toSet)
    }

  //////////////////////////////////////////////////////////////////////////

  def simplify(ast: PEGAst): PEGAst =
    ast match {
      case Cat(x :: Nil) => x
      case Cat(Nil) => Empty
      case Cat(xs) => Cat(xs.map(simplify))

      case Alt(x :: Nil) => x
      case Alt(Nil) => Empty
      case Alt(xs) => Alt(xs.map(simplify))

      case Star(Star(x))     => Star(x)
      case Star(Plus(x))     => Star(x)
      case Star(Optional(x)) => Star(x)
      case Star(x)           => Star(simplify(x))


      case Plus(Star(x))     => Star(x)
      case Plus(Plus(x))     => Plus(x)
      case Plus(Optional(x)) => Star(x)
      case Plus(x)           => Plus(simplify(x))

      case Optional(Star(x))     => Star(x)
      case Optional(Plus(x))     => Star(x)
      case Optional(Optional(x)) => Optional(x)

      case PosLook(NegLook(x)) => NegLook(x)
      case PosLook(PosLook(x)) => PosLook(x)
      case PosLook(x)          => PosLook(simplify(x))

      case NegLook(NegLook(x)) => PosLook(x)
      case NegLook(PosLook(x)) => NegLook(x)
      case NegLook(x)          => NegLook(simplify(x))

      case _ => ast
    }

  def normal(ast: PEGAst): PEGAst = {
    var last,current = ast
    do{
      last = current
      current = simplify(last)
    }while(last != current)
    current
  }

  def flattenNoWS(tree: PTree): String  =
    tree match {
      case PEmpty => ""
      case PLeaf(node) => node
      case PBranch("Spacing" , _) => ""
      case PBranch(_, children) =>
        children.map(flattenNoWS)
          .mkString("")
    }

  def flattenChars(tree:PTree): List[Char] =
    tree match {
      case PEmpty => List.empty
      case PLeaf(node) => List(node(0))

      case PBranch("Range", children) => children match{
        case List(first,_,last) =>
          flatten(first)(0)
            .to(flatten(last)(0))
            .toList
      }

      case PBranch("Char",List(escape,char)) =>
        val c = flatten(char)(0)
        escape match {
          case PEmpty => List(c)
          case _ => List(c match {
              case 'n' => '\n'
              case 'r' => '\r'
              case 't' => '\t'
              case '\'' => '\''
              case '\\' => '\\'
              case _ => c
            })
        }

      case PBranch(_, cs) => cs
        .map(flattenChars)
        .fold(List.empty){_ ++ _}
    }

  def toAst(grammer: PTree): Map[String,PEGAst] = {
    var last = grammer2ast(grammer)
    var current = last.view.mapValues{simplify}.toMap
    while( last != current ){
      last = current
      current = last.view.mapValues{simplify}.toMap
    }
    current
  }

  val source: String =
    """
      | # Heierarchical syntax
      |
      | Grammer    <- Spacing Definition+ EndOfFile
      | Definition <- Identifier LEFTARROW Expression
      |
      | Expression <- Sequence (SLASH Sequence)*
      | Sequence   <- Prefix*
      | Prefix     <- (AND / NOT)? Suffix
      | Suffix     <- Primary (QUESTION / STAR / PLUS)?
      | Primary    <- Identifier !LEFTARROW
      |             / OPEN Expression CLOSE
      |             / Literal / Class / DOT
      |
      | # Lexical syntax
      | Identifier <- IdentStart IdentCont* Spacing
      | IdentStart <- [a-zA-Z_]
      | IdentCont  <- IdentStart / [0-9]
      |
      | Literal <- ['] (!['] Char)* ['] Spacing
      |          / ["] (!["] Char)* ["] Spacing
      |          / [`] (![`] Char)* [`] Spacing
      | Class   <- '[' (!']' Range)* ']' Spacing
      | Range   <- Char '-' Char / Char
      | Char    <- '\\' [nrt'"\[\]\\]
      |          / !'\\' .
      |
      | LEFTARROW <- '<-' Spacing
      | SLASH     <- '/' Spacing
      | AND       <- '&' Spacing
      | NOT       <- '!' Spacing
      | QUESTION  <- '?' Spacing
      | STAR      <- '*' Spacing
      | PLUS      <- '+' Spacing
      | OPEN      <- '(' Spacing
      | CLOSE     <- ')' Spacing
      | DOT       <- '.' Spacing
      |
      | Spacing   <- (Space / Comment)*
      | Comment   <- '#' (!EndOfLine .)* EndOfLine
      | Space     <- ' ' / '\t' / EndOfLine
      | EndOfLine <- '\r\n' / '\n' / '\r'
      | EndOfFile <- !.
      |
      |""".stripMargin

}

/**
 * Map(
 *  A  -> Alt(List(Cat(List(Lit(List(', a, ')), Var(A), Lit(List(', b, ')))), PEG.ast.Empty$@2ef5e5e3)), B  -> Alt(List(Cat(List(Lit(List(', b, ')), Var(B), Lit(List(', c, ')))), PEG.ast.Empty$@2ef5e5e3)), D  -> Cat(List(PosLook(Cat(List(Var(A), NegLook(Lit(List(', b, ')))))), Star(Var(a)), Var(B), NegLook(PEG.ast.Any$@36d4b5c))))
 */
