package PEG.lexparse

class Lexer(val source: String) {

  private var pos: Int = 0

  def mark: Int = pos
  def reset(i:Int): Unit = {pos = i}

  def hasMore: Boolean =
    pos < source.length

  def peek: Char =
    source(pos)

  def consume: Char = {
    val c = peek
    pos += 1
    c
  }
}
