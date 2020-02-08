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
catPart_6 <- {
val pos_9= mark
val res_10 = for{
char_11 <- expect('a')
 } yield PBranch("Lit",Seq( PLeaf(char_11.toString) )) 
res_10.recoverWith{ case p: ParseError =>
reset(pos_9)
 Failure( p ~ ParseFailed("expected 'a'",pos_9) ) 
}
}
catPart_7 <- {
val pos_12 = mark
A().recoverWith{ case p: ParseError =>
reset(pos_12)
 Failure( p ~ ParseFailed("expected Var 'A'",pos_12) ) 
}
}
catPart_8 <- {
val pos_13= mark
val res_14 = for{
char_15 <- expect('b')
 } yield PBranch("Lit",Seq( PLeaf(char_15.toString) )) 
res_14.recoverWith{ case p: ParseError =>
reset(pos_13)
 Failure( p ~ ParseFailed("expected 'b'",pos_13) ) 
}
}
 }  yield PBranch("A",Seq( catPart_6,catPart_7,catPart_8 )) 
res_5.recoverWith{ case p: ParseError => 
reset(pos0_4)
Failure( p  )
}
}
res_2.recoverWith{ case err_3: ParseError =>
reset(pos_1)
val res_16 = {
 Try(PEmpty) 
}
res_16.recoverWith{ case err_17: ParseError =>
reset(pos_1)
Failure(err_3 ~ err_17 ~  ParseFailed("",pos_1) )
}
}
}


def B(): Try[PTree] = {
val pos_18 = mark
val res_19 = {
val pos0_21 = mark
val res_22 = for{
catPart_23 <- {
val pos_26= mark
val res_27 = for{
char_28 <- expect('b')
 } yield PBranch("Lit",Seq( PLeaf(char_28.toString) )) 
res_27.recoverWith{ case p: ParseError =>
reset(pos_26)
 Failure( p ~ ParseFailed("expected 'b'",pos_26) ) 
}
}
catPart_24 <- {
val pos_29 = mark
B().recoverWith{ case p: ParseError =>
reset(pos_29)
 Failure( p ~ ParseFailed("expected Var 'B'",pos_29) ) 
}
}
catPart_25 <- {
val pos_30= mark
val res_31 = for{
char_32 <- expect('c')
 } yield PBranch("Lit",Seq( PLeaf(char_32.toString) )) 
res_31.recoverWith{ case p: ParseError =>
reset(pos_30)
 Failure( p ~ ParseFailed("expected 'c'",pos_30) ) 
}
}
 }  yield PBranch("B",Seq( catPart_23,catPart_24,catPart_25 )) 
res_22.recoverWith{ case p: ParseError => 
reset(pos0_21)
Failure( p  )
}
}
res_19.recoverWith{ case err_20: ParseError =>
reset(pos_18)
val res_33 = {
 Try(PEmpty) 
}
res_33.recoverWith{ case err_34: ParseError =>
reset(pos_18)
Failure(err_20 ~ err_34 ~  ParseFailed("",pos_18) )
}
}
}


def D(): Try[PTree] = {
val pos0_35 = mark
val res_36 = for{
catPart_37 <- {
val pos_41 = mark
val res_42 = {
val pos_43 = mark
val res_44 = {
val pos0_45 = mark
val res_46 = for{
catPart_47 <- {
val pos_49 = mark
A().recoverWith{ case p: ParseError =>
reset(pos_49)
 Failure( p ~ ParseFailed("expected Var 'A'",pos_49) ) 
}
}
catPart_48 <- {
val pos_50 = mark
val res_51 = {
val pos_52= mark
val res_53 = for{
char_54 <- expect('b')
 } yield PBranch("Lit",Seq( PLeaf(char_54.toString) )) 
res_53.recoverWith{ case p: ParseError =>
reset(pos_52)
 Failure( p ~ ParseFailed("expected 'b'",pos_52) ) 
}
}
reset(pos_50)
if( res_51.isSuccess ) Failure(ParseFailed("Neglook failed",pos_50))
 else { Try(PEmpty) }
}
 }  yield PBranch("catPart_37",Seq( catPart_47,catPart_48 )) 
res_46.recoverWith{ case p: ParseError => 
reset(pos0_45)
Failure( p  )
}
}
reset(pos_43)
if( res_44.isSuccess ) Failure(ParseFailed("Neglook failed",pos_43))
 else { Try(PEmpty) }
}
reset(pos_41)
if( res_42.isSuccess ) Failure(ParseFailed("Neglook failed",pos_41))
 else { Try(PEmpty) }
}
catPart_38 <- {
var parts_55 = ArrayBuffer.empty[PTree]
var pos_56 = mark
var res_57 = {
val pos_58= mark
val res_59 = for{
char_60 <- expect('a')
 } yield PBranch("Lit",Seq( PLeaf(char_60.toString) )) 
res_59.recoverWith{ case p: ParseError =>
reset(pos_58)
 Failure( p ~ ParseFailed("expected 'a'",pos_58) ) 
}
}
res_57.recover{ _ => reset(pos_56) }
while(res_57.isSuccess){
parts_55 += res_57.get
pos_56 = mark
res_57 = {
val pos_61= mark
val res_62 = for{
char_63 <- expect('a')
 } yield PBranch("Lit",Seq( PLeaf(char_63.toString) )) 
res_62.recoverWith{ case p: ParseError =>
reset(pos_61)
 Failure( p ~ ParseFailed("expected 'a'",pos_61) ) 
}
}
res_57.recover{ _ => reset(pos_56) }
}
 Try(PBranch("catPart_38",parts_55.toSeq)) 
}
catPart_39 <- {
val pos_64 = mark
B().recoverWith{ case p: ParseError =>
reset(pos_64)
 Failure( p ~ ParseFailed("expected Var 'B'",pos_64) ) 
}
}
catPart_40 <- {
val pos_65 = mark
EOF().recoverWith{ case p: ParseError =>
reset(pos_65)
 Failure( p ~ ParseFailed("expected Var 'EOF'",pos_65) ) 
}
}
 }  yield PBranch("D",Seq( catPart_37,catPart_38,catPart_39,catPart_40 )) 
res_36.recoverWith{ case p: ParseError => 
reset(pos0_35)
Failure( p  )
}
}


def EOF(): Try[PTree] = {
val pos_66 = mark
val res_67 = {
val pos_68 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError =>
reset(pos_68)
 Failure( p ~ ParseFailed("Expected any char",pos_68) )  
}
}
reset(pos_66)
if( res_67.isSuccess ) Failure(ParseFailed("Neglook failed",pos_66))
 else { Try(PEmpty) }
}
}
