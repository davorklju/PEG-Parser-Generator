package PEG.generators

import PEG.PEGParser.GeneratedPEGParser
import PEG.data._
import PEG.lexparse.Lexer

object PEGGenerator extends ParserGenerator {
  val path = """C:\Users\Davor\IdeaProjects\PEG-Parser\src\main\scala\"""
  override val pack: String = "PEG.PEGParser"
  override val name: String = "GeneratedPEGParser"


  def main(args: Array[String]): Unit = {
    val lexer = new Lexer(source)
    val parser = new GeneratedPEGParser(lexer)

    val g = parser.Grammar().map(toAst)

    if(g.isFailure) println("parse failed")

    g.foreach{ grammar =>
      println("parse complete")
      this.genParserToFile(grammar)
      println("gen complete")
    }

  }

  def tree2grammar(grammar: PTree): List[Definition] =
    grammar match {
      case PEmpty => throw new Error()
      case PLeaf(_) => throw new Error()

      case PBranch("Grammar", List(_, PBranch(_, List(x0, rest)), _)) =>
        val xs = rest match {
          case PBranch(_, Nil) => List.empty
          case PBranch(_, xs) => xs
        }
        (x0 :: xs.toList).map(tree2Def)
    }

  def tree2Def(definition: PTree): Definition =
    definition match {
      case PEmpty => throw new Error()
      case PLeaf(_) => throw new Error()
      case PBranch("Definition", List(ident,memo,_,expression)) =>
        val id = flattenNoWS(ident)
        val expr = tree2ast(expression)
        memo match{
          case PEmpty =>
            Definition(id,memo = false,expr)
          case PBranch("STAR",_) =>
            Definition(id,memo = true,expr)
        }
      }

  def tree2ast(tree: PTree): PEGAst =
    tree match {
      case PEmpty => throw new Error()
      case PLeaf(_) => throw new Error()


      case PBranch("Expression", List(e0, PBranch(_,ls))) =>
        val es = ls.toList.map {
          case PBranch(_,List (PBranch("SLASH", _), e)) => e
        }
        if(es.isEmpty) tree2ast(e0)
        else Alt((e0 :: es).map(tree2ast))

      case PBranch("Sequence",List(PBranch(_, ss),act)) =>
        val ast = ss match {
          case Nil => PEG.data.Empty
          case x :: Nil => tree2ast(x)
          case _  => Cat(ss.map(tree2ast))
        }
        flattenAction(act)
          .map{ case (a,b,c) => Action(ast,a,b,c)}
          .getOrElse(ast)

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

      case PBranch("Identifier", List(start,cont,_)) =>
        Var( s"${flattenNoWS(start)}${flattenNoWS(cont)}" )

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

  def flattenAction(tree: PTree): Option[(String,List[String],String)] =
    tree match {
      case PEmpty => Option.empty
      case PBranch("Action", List(_,reType,_,PBranch(_,args),_,body,_)) =>
        val list: List[String] = args.map{flattenNoWS}.toList
        val list2 = if(list == List("")) Nil else list
        val str = flattenNoWS(reType).replaceAll(" ", "")
        Option((str,list2,flatten(body)))
      case _ => throw new Error()
    }

  def toAst(grammer: PTree): List[Definition] =
    tree2grammar(grammer).map{
      case Definition(name,memo,ast) =>
        Definition(name,memo,normal(ast))
    }

  val source: String =
    """
      | # Heierarchical syntax
      |
      | Grammar   <- Spacing Definition+ EndOfFile
      | Definition <- Identifier STAR? LEFTARROW Expression
      |
      | Expression <- Sequence (SLASH Sequence)*
      | Sequence   <- Prefix* Action?
      | Prefix     <- (AND / NOT)? Suffix
      | Suffix     <- Primary (QUESTION / STAR / PLUS)?
      | Primary*    <- Identifier !(STAR? LEFTARROW)
      |             / OPEN Expression CLOSE
      |             / Literal / Class / DOT
      |
      | # Lexical syntax
      | Identifier <- IdentStart IdentCont* Spacing
      | IdentStart <- [a-zA-Z_]
      | IdentCont  <- IdentStart / [0-9]
      |
      | Action         <- OPENCURLY (![|}] .)+ VERTBAR ActionIdent* VERTBAR (![}] .)* CLOSECURLY
      |
      | ActionIdent <- Identifier OPEN ActionIdent (COMMA ActionIdent)* CLOSE
      |              / Identifier
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
      | OPENCURLY <- '{' Spacing
      | CLOSECURLY<- '}' Spacing
      | VERTBAR   <- '|' Spacing
      | COMMA     <- ',' Spacing
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
