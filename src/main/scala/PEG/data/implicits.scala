package PEG.data


object implicits{
  implicit class ParseErrorImpl[t](val self: ParseError[t]){
    def ~(rhs: ParseError[t]): ParseError[t] = (self,rhs) match {
      case (ParseFailed(m1,p1),ParseFailed(m2,p2)) =>
        ParseFailed(s"$m1 at $p1\n$m2",p2).asInstanceOf[ParseError[t]]
      case (p,ParseFailed(msg,pos)) => ParseFailed(s"${p.getMessage}\n$msg",pos)
      case (ParseFailed(msg,pos),p) => ParseFailed(s"$msg at $pos\n${p.getMessage}",p.pos)
      case (p1,p2) => ParseFailed(s"${p1.getMessage}${p2.getMessage}",p2.pos)
    }
  }
}
