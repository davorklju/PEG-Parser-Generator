package PEG.PEGParser
import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class CSGGenerated(lexer: Lexer) extends Parser(lexer){


def A(): Try[PTree] = {
val pos_1 = mark
val res_2 = {
val pos0_4 = mark
val res_5 = for{
catPart_6 <- expect('a').map{ char_9 => PLeaf(char_9.toString) }
catPart_7 <- A()
catPart_8 <- expect('b').map{ char_10 => PLeaf(char_10.toString) }
 }  yield PBranch("A",Seq( catPart_6,catPart_7,catPart_8 )) 
res_5.recoverWith{ case p: ParseError => 
reset(pos0_4)
Failure( p  )
}
}
res_2.recoverWith{ case err_3: ParseError =>
reset(pos_1)
 Try(PEmpty) 
}
}


def B(): Try[PTree] = {
val pos_11 = mark
val res_12 = {
val pos0_14 = mark
val res_15 = for{
catPart_16 <- expect('b').map{ char_19 => PLeaf(char_19.toString) }
catPart_17 <- B()
catPart_18 <- expect('c').map{ char_20 => PLeaf(char_20.toString) }
 }  yield PBranch("B",Seq( catPart_16,catPart_17,catPart_18 )) 
res_15.recoverWith{ case p: ParseError => 
reset(pos0_14)
Failure( p  )
}
}
res_12.recoverWith{ case err_13: ParseError =>
reset(pos_11)
 Try(PEmpty) 
}
}


def D(): Try[PTree] = {
val pos0_21 = mark
val res_22 = for{
catPart_23 <- {
val pos_27 = mark
val res_28 = {
val pos_29 = mark
val res_30 = {
val pos0_31 = mark
val res_32 = for{
catPart_33 <- A()
catPart_34 <- {
val pos_35 = mark
val res_36 = {
val pos_37= mark
val res_38 = for{
char_part_39 <- expect('b')
 } yield PBranch("Lit",Seq( PLeaf(char_part_39.toString) )) 
res_38.recoverWith{ case p: ParseError =>
reset(pos_37)
 Failure( p ~ ParseFailed("expected 'b'",pos_37) ) 
}
}
reset(pos_35)
if( res_36.isSuccess ) Failure(ParseFailed("Neglook failed",pos_35))
 else { Try(PEmpty) }
}
 }  yield PBranch("catPart_23",Seq( catPart_33,catPart_34 )) 
res_32.recoverWith{ case p: ParseError => 
reset(pos0_31)
Failure( p  )
}
}
reset(pos_29)
if( res_30.isSuccess ) Failure(ParseFailed("Neglook failed",pos_29))
 else { Try(PEmpty) }
}
reset(pos_27)
if( res_28.isSuccess ) Failure(ParseFailed("Neglook failed",pos_27))
 else { Try(PEmpty) }
}
catPart_24 <- {
def subMatch_43 = {
val pos_44= mark
val res_45 = for{
char_part_46 <- expect('a')
 } yield PBranch("Lit",Seq( PLeaf(char_part_46.toString) )) 
res_45.recoverWith{ case p: ParseError =>
reset(pos_44)
 Failure( p ~ ParseFailed("expected 'a'",pos_44) ) 
}
}
var buf_40 = ArrayBuffer.empty[PTree]
var pos_41 = mark
var res_42 = subMatch_43
res_42.recover{ _ => reset(pos_41) }
while(res_42.isSuccess){
buf_40 += res_42.get
pos_41 = mark
res_42 = subMatch_43
res_42.recover{ _ => reset(pos_41) }
}
 Try(PBranch("catPart_24",buf_40.toSeq)) 
}
catPart_25 <- B()
catPart_26 <- EOF()
 }  yield PBranch("D",Seq( catPart_23,catPart_24,catPart_25,catPart_26 )) 
res_22.recoverWith{ case p: ParseError => 
reset(pos0_21)
Failure( p  )
}
}


def EOF(): Try[PTree] = {
val pos_47 = mark
val res_48 = {
val pos_49 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError =>
reset(pos_49)
 Failure( p ~ ParseFailed("Expected any char",pos_49) )  
}
}
reset(pos_47)
if( res_48.isSuccess ) Failure(ParseFailed("Neglook failed",pos_47))
 else { Try(PEmpty) }
}
}
