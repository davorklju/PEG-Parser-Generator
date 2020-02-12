package PEG.data

sealed trait PTree

object PEmpty extends PTree
case class PLeaf(node: String) extends PTree
case class PBranch(node: String, children: Seq[PTree]) extends PTree







