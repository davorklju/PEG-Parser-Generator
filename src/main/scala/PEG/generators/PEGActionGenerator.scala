package PEG.generators

import PEG.PEGParser.GeneratedPEGParser
import PEG.lexparse.Lexer

import scala.util.{Failure, Success}

object PEGActionGenerator extends ParserGenerator {
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\main\scala\"""
  override val pack: String = "PEG.PEGParser"
  override val name: String = "GeneratedPEGActionParser"

  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)
    val result = parser.Grammar().map{PEGGenerator.toAst}
    result match {
      case Failure(exception) => throw exception
      case Success(value) =>
        println("parse done")
        this.genParserToFile(value, List("PEG.generators.PEGActionGenerator"))
        println("gen done")
    }
  }

  def escapeChar(s: String): Char = s match {
    case "n" => '\n'
    case "r" => '\r'
    case "t" => '\t'
    case "'" => '\''
    case """""" => '\"'
    case "[" => '['
    case "]" => ']'
    case _ => s(0)
  }

  val source: String =
    """
      |
      | Grammar <- Spacing Definition+ EndOfFile { List[Definition] | _ ds _ | ds }
      |
      | ##################################################################################3
      |
      | Definition <- Identifier STAR? LEFTARROW Expression { Definition | Var(id) memo _ e |
      |                       PEG.data.Definition(id,memo != PEmpty,e)
      |                }
      |
      | Expression <- Sequence (SLASH Sequence {PEGAst |_ s|s})* { PEGAst | s ss |
      |                       if(ss.isEmpty) s else Alt(s :: ss)
      |                }
      |
      | Sequence <- Prefix* SymAction? { PEGAst | ps act  |
      |                       val ast = if(ps.isEmpty) Empty else if(ps.length == 1) ps.head else Cat(ps)
      |                       if( act == Option.empty) ast
      |                       else Action(ast,act.get._1,act.get._2,act.get._3)
      |                 }
      |
      | ################################################################################
      |
      | SymAction <- CURRLYOPEN  (![|] Char {Char | _ c | c})+
      |              VERTBAR     SymActionArg+
      |              VERTBAR     (![}] Char {Char | _ c| c})+
      |              CURRLYCLOSE  { (String,List[String],String) | _ _type _ args _ body _ |
      |                   (_type.mkString(""),args,body.mkString(""))
      |              }
      |
      | SymActionArg <- (![| \t\n] Char { Char | _ c | c })+ Spacing { String | s _ | s.mkString("") }
      |
      | ################################################################################
      |
      | Prefix <- AND Suffix { PEGAst | _ s | PosLook(s) }
      |         / NOT Suffix { PEGAst | _ s | NegLook(s) }
      |         / Suffix
      |
      | Suffix <- Primary STAR     { PEGAst | p _ | Star(p)     }
      |         / Primary PLUS     { PEGAst | p _ | Plus(p)     }
      |         / Primary QUESTION { PEGAst | p _ | Optional(p) }
      |         / Primary          { PEGAst | p   | p           }
      |
      | Primary* <- Identifier !(STAR? LEFTARROW) {PEGAst | id _  | id }
      |           / OPEN Expression CLOSE         {PEGAst | _ e _ | e}
      |           / DOT                           {PEGAst | _ | Any }
      |           / Literal
      |           / CharClass
      | #################################################################################
      |
      | Identifier <- IdentStart IdentPart* Spacing { PEGAst | s p _ | Var((s::p).mkString("")) }
      |
      | IdentStart <- [A-Z_]     { Char | PLeaf(c) | c(0) }
      | IdentPart  <- IdentStart
      |             / [a-z]      { Char | PLeaf(c) | c(0) }
      |             / [0-9]      {Char | PLeaf(c) | c(0) }
      |
      | ###################################################################################
      |
      | Literal <- ['] (!['] Char {Char| _ c| c})* ['] Spacing { PEGAst | _ l _ _ | Lit(l) }
      |          / ["] (!["] Char {Char| _ c| c})* ["] Spacing { PEGAst | _ l _ _ | Lit(l) }
      |          / [`] (![`] Char {Char| _ c| c})* [`] Spacing { PEGAst | _ l _ _ | Lit(l) }
      |
      | ###################################################################################
      |
      | CharClass <- '[' ClassPart* ']' Spacing {PEGAst | _ cs _ _ |  Class(cs.fold(Set.empty)(_ union _)) }
      |
      | ClassPart <- ![\]] Range {Set[Char]| _ s | s}
      |
      | Range <- Char '-' Char  {Set[Char] | c0 _ c1 | c0.to(c1).toSet }
      |        / Char           {Set[Char] | c       | Set(c) }
      |
      | Char <- '\\' [nrt'"\[\]\\] { Char | _ PLeaf(c) | PEGActionGenerator.escapeChar(c)  }
      |       / !'\\' .            { Char | _ PLeaf(c) | c(0) }
      |
      | LEFTARROW <- '<-' Spacing
      | OPEN      <- '(' Spacing
      | CLOSE     <- ')' Spacing
      | DOT       <- '.' Spacing
      | STAR      <- '*' Spacing
      | PLUS      <- '+' Spacing
      | QUESTION  <- '?' Spacing
      | AND       <- '&' Spacing
      | NOT       <- '!' Spacing
      | SLASH     <- '/' Spacing
      |
      | CURRLYOPEN  <- '{' Spacing
      | CURRLYCLOSE <- '}' Spacing
      | VERTBAR     <- '|' Spacing
      | COMMA       <- ',' Spacing
      |
      | Spacing   <- (Space / Comment)*
      | Comment   <- '#' (!EndOfLine .)* EndOfLine
      | Space     <- ' ' / '\t' / EndOfLine
      | EndOfLine <- '\r\n' / '\n' / '\r'
      | EndOfFile <- !.
      |
      |""".stripMargin

}
