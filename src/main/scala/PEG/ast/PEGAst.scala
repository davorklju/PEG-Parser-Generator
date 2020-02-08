package PEG.ast

sealed trait PEGAst
case class Lit(chars: Seq[Char]) extends PEGAst
case class Var(name: String) extends PEGAst

case class Class(chars: Set[Char]) extends PEGAst

object Any extends PEGAst
object Empty extends PEGAst

case class Star(ast: PEGAst) extends PEGAst
case class Plus(ast: PEGAst) extends PEGAst
case class Optional(ast: PEGAst) extends PEGAst

case class Cat(asts: Seq[PEGAst]) extends PEGAst
case class Alt(asts: Seq[PEGAst]) extends PEGAst

case class PosLook(ast: PEGAst) extends PEGAst
case class NegLook(ast: PEGAst) extends PEGAst

