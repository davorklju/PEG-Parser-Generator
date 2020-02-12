package PEG.data



sealed abstract class ParseError[+t](errMsg: String) extends Exception(errMsg){
  val pos: Int
}

case class UnexpectedEOF(pos: Int)
  extends ParseError[Nothing](s"UnExpected at $pos")

case class ExpectedOneOf[t](chars: List[t], pos: Int)
  extends ParseError[t]( s"ExpectedOneOf ${chars.mkString(",")} at $pos")

case class NotExpectedOneOf[t](chars: List[t], pos: Int)
  extends ParseError[t]( s"Not ExpectedOneOf ${chars.mkString(",")} at $pos")

case class ParseFailed(reason: String, pos: Int)
  extends ParseError[Nothing](s"$reason at $pos")
