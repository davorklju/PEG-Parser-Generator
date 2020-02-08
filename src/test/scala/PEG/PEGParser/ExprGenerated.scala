package PEG.PEGParser
import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class ExprGenerated(lexer: Lexer) extends Parser(lexer){


def Term(): Try[PTree] = {
val pos0_1 = mark
val res_2 = for{
catPart_3 <- {
val pos_5 = mark
val res_6 = {
val pos_8= mark
val res_9 = for{
char_10 <- expect('-')
 } yield PBranch("Lit",Seq( PLeaf(char_10.toString) )) 
res_9.recoverWith{ case p: ParseError =>
reset(pos_8)
 Failure( p ~ ParseFailed("expected '-'",pos_8) ) 
}
}
res_6.recoverWith{ case err_7: ParseError =>
reset(pos_5)
val res_11 = {
 Try(PEmpty) 
}
res_11.recoverWith{ case err_12: ParseError =>
reset(pos_5)
Failure(err_7 ~ err_12 ~  ParseFailed("",pos_5) )
}
}
}
catPart_4 <- {
val pos_13 = mark
Lit().recoverWith{ case p: ParseError =>
reset(pos_13)
 Failure( p ~ ParseFailed("expected Var 'Lit'",pos_13) ) 
}
}
 }  yield PBranch("Term",Seq( catPart_3,catPart_4 )) 
res_2.recoverWith{ case p: ParseError => 
reset(pos0_1)
Failure( p  )
}
}


def Expr(): Try[PTree] = {
val pos_14 = mark
val res_15 = {
val pos0_17 = mark
val res_18 = for{
catPart_19 <- {
val pos_22 = mark
Fact().recoverWith{ case p: ParseError =>
reset(pos_22)
 Failure( p ~ ParseFailed("expected Var 'Fact'",pos_22) ) 
}
}
catPart_20 <- {
val pos_23 = mark
PLUS().recoverWith{ case p: ParseError =>
reset(pos_23)
 Failure( p ~ ParseFailed("expected Var 'PLUS'",pos_23) ) 
}
}
catPart_21 <- {
val pos_24 = mark
Expr().recoverWith{ case p: ParseError =>
reset(pos_24)
 Failure( p ~ ParseFailed("expected Var 'Expr'",pos_24) ) 
}
}
 }  yield PBranch("Expr",Seq( catPart_19,catPart_20,catPart_21 )) 
res_18.recoverWith{ case p: ParseError => 
reset(pos0_17)
Failure( p  )
}
}
res_15.recoverWith{ case err_16: ParseError =>
reset(pos_14)
val res_25 = {
val pos_27 = mark
Fact().recoverWith{ case p: ParseError =>
reset(pos_27)
 Failure( p ~ ParseFailed("expected Var 'Fact'",pos_27) ) 
}
}
res_25.recoverWith{ case err_26: ParseError =>
reset(pos_14)
Failure(err_16 ~ err_26 ~  ParseFailed("",pos_14) )
}
}
}


def PLUS(): Try[PTree] = {
val pos0_28 = mark
val res_29 = for{
catPart_30 <- {
val pos_32= mark
val res_33 = for{
char_34 <- expect('+')
 } yield PBranch("Lit",Seq( PLeaf(char_34.toString) )) 
res_33.recoverWith{ case p: ParseError =>
reset(pos_32)
 Failure( p ~ ParseFailed("expected '+'",pos_32) ) 
}
}
catPart_31 <- {
val pos_35 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_35)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_35) ) 
}
}
 }  yield PBranch("PLUS",Seq( catPart_30,catPart_31 )) 
res_29.recoverWith{ case p: ParseError => 
reset(pos0_28)
Failure( p  )
}
}


def CLOSE(): Try[PTree] = {
val pos0_36 = mark
val res_37 = for{
catPart_38 <- {
val pos_40= mark
val res_41 = for{
char_42 <- expect(')')
 } yield PBranch("Lit",Seq( PLeaf(char_42.toString) )) 
res_41.recoverWith{ case p: ParseError =>
reset(pos_40)
 Failure( p ~ ParseFailed("expected ')'",pos_40) ) 
}
}
catPart_39 <- {
val pos_43 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_43)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_43) ) 
}
}
 }  yield PBranch("CLOSE",Seq( catPart_38,catPart_39 )) 
res_37.recoverWith{ case p: ParseError => 
reset(pos0_36)
Failure( p  )
}
}


def Fact(): Try[PTree] = {
val pos_44 = mark
val res_45 = {
val pos0_47 = mark
val res_48 = for{
catPart_49 <- {
val pos_52 = mark
Term().recoverWith{ case p: ParseError =>
reset(pos_52)
 Failure( p ~ ParseFailed("expected Var 'Term'",pos_52) ) 
}
}
catPart_50 <- {
val pos_53 = mark
PROD().recoverWith{ case p: ParseError =>
reset(pos_53)
 Failure( p ~ ParseFailed("expected Var 'PROD'",pos_53) ) 
}
}
catPart_51 <- {
val pos_54 = mark
Fact().recoverWith{ case p: ParseError =>
reset(pos_54)
 Failure( p ~ ParseFailed("expected Var 'Fact'",pos_54) ) 
}
}
 }  yield PBranch("Fact",Seq( catPart_49,catPart_50,catPart_51 )) 
res_48.recoverWith{ case p: ParseError => 
reset(pos0_47)
Failure( p  )
}
}
res_45.recoverWith{ case err_46: ParseError =>
reset(pos_44)
val res_55 = {
val pos_57 = mark
Term().recoverWith{ case p: ParseError =>
reset(pos_57)
 Failure( p ~ ParseFailed("expected Var 'Term'",pos_57) ) 
}
}
res_55.recoverWith{ case err_56: ParseError =>
reset(pos_44)
Failure(err_46 ~ err_56 ~  ParseFailed("",pos_44) )
}
}
}


def OPEN(): Try[PTree] = {
val pos0_58 = mark
val res_59 = for{
catPart_60 <- {
val pos_62= mark
val res_63 = for{
char_64 <- expect('(')
 } yield PBranch("Lit",Seq( PLeaf(char_64.toString) )) 
res_63.recoverWith{ case p: ParseError =>
reset(pos_62)
 Failure( p ~ ParseFailed("expected '('",pos_62) ) 
}
}
catPart_61 <- {
val pos_65 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_65)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_65) ) 
}
}
 }  yield PBranch("OPEN",Seq( catPart_60,catPart_61 )) 
res_59.recoverWith{ case p: ParseError => 
reset(pos0_58)
Failure( p  )
}
}


def PROD(): Try[PTree] = {
val pos0_66 = mark
val res_67 = for{
catPart_68 <- {
val pos_70= mark
val res_71 = for{
char_72 <- expect('*')
 } yield PBranch("Lit",Seq( PLeaf(char_72.toString) )) 
res_71.recoverWith{ case p: ParseError =>
reset(pos_70)
 Failure( p ~ ParseFailed("expected '*'",pos_70) ) 
}
}
catPart_69 <- {
val pos_73 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_73)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_73) ) 
}
}
 }  yield PBranch("PROD",Seq( catPart_68,catPart_69 )) 
res_67.recoverWith{ case p: ParseError => 
reset(pos0_66)
Failure( p  )
}
}


def Lit(): Try[PTree] = {
val pos_74 = mark
val res_75 = {
val pos0_77 = mark
val res_78 = for{
catPart_79 <- {
val pos_82 = mark
OPEN().recoverWith{ case p: ParseError =>
reset(pos_82)
 Failure( p ~ ParseFailed("expected Var 'OPEN'",pos_82) ) 
}
}
catPart_80 <- {
val pos_83 = mark
Expr().recoverWith{ case p: ParseError =>
reset(pos_83)
 Failure( p ~ ParseFailed("expected Var 'Expr'",pos_83) ) 
}
}
catPart_81 <- {
val pos_84 = mark
CLOSE().recoverWith{ case p: ParseError =>
reset(pos_84)
 Failure( p ~ ParseFailed("expected Var 'CLOSE'",pos_84) ) 
}
}
 }  yield PBranch("Lit",Seq( catPart_79,catPart_80,catPart_81 )) 
res_78.recoverWith{ case p: ParseError => 
reset(pos0_77)
Failure( p  )
}
}
res_75.recoverWith{ case err_76: ParseError =>
reset(pos_74)
val res_85 = {
val pos_87 = mark
Int().recoverWith{ case p: ParseError =>
reset(pos_87)
 Failure( p ~ ParseFailed("expected Var 'Int'",pos_87) ) 
}
}
res_85.recoverWith{ case err_86: ParseError =>
reset(pos_74)
Failure(err_76 ~ err_86 ~  ParseFailed("",pos_74) )
}
}
}


def Int(): Try[PTree] = {
val pos0_88 = mark
val res_89 = for{
catPart_90 <- {
val pos0_92 = mark
val res_93 = for{
catPart_94 <- {
val pos_96 = mark
val res_97 = for{
char_98 <- expect('6','9','2','8','4','0','5','1','7','3')
 } yield PLeaf(char_98.toString) 
res_97.recoverWith{ case p: ParseError =>
reset(pos_96)
 Failure( p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'",pos_96) ) 
}
}
catPart_95 <- {
var parts_99 = ArrayBuffer.empty[PTree]
var pos_100 = mark
var res_101 = {
val pos_102 = mark
val res_103 = for{
char_104 <- expect('6','9','2','8','4','0','5','1','7','3')
 } yield PLeaf(char_104.toString) 
res_103.recoverWith{ case p: ParseError =>
reset(pos_102)
 Failure( p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'",pos_102) ) 
}
}
res_101.recover{ _ => reset(pos_100) }
while(res_101.isSuccess){
parts_99 += res_101.get
pos_100 = mark
res_101 = {
val pos_105 = mark
val res_106 = for{
char_107 <- expect('6','9','2','8','4','0','5','1','7','3')
 } yield PLeaf(char_107.toString) 
res_106.recoverWith{ case p: ParseError =>
reset(pos_105)
 Failure( p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'",pos_105) ) 
}
}
res_101.recover{ _ => reset(pos_100) }
}
 Try(PBranch("catPart_95",parts_99.toSeq)) 
}
 }  yield PBranch("catPart_90",Seq( catPart_94,catPart_95 )) 
res_93.recoverWith{ case p: ParseError => 
reset(pos0_92)
Failure( p  )
}
}
catPart_91 <- {
val pos_108 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_108)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_108) ) 
}
}
 }  yield PBranch("Int",Seq( catPart_90,catPart_91 )) 
res_89.recoverWith{ case p: ParseError => 
reset(pos0_88)
Failure( p  )
}
}


def Stmt(): Try[PTree] = {
val pos0_109 = mark
val res_110 = for{
catPart_111 <- {
val pos_114 = mark
WS().recoverWith{ case p: ParseError =>
reset(pos_114)
 Failure( p ~ ParseFailed("expected Var 'WS'",pos_114) ) 
}
}
catPart_112 <- {
val pos_115 = mark
Expr().recoverWith{ case p: ParseError =>
reset(pos_115)
 Failure( p ~ ParseFailed("expected Var 'Expr'",pos_115) ) 
}
}
catPart_113 <- {
val pos_116 = mark
EOF().recoverWith{ case p: ParseError =>
reset(pos_116)
 Failure( p ~ ParseFailed("expected Var 'EOF'",pos_116) ) 
}
}
 }  yield PBranch("Stmt",Seq( catPart_111,catPart_112,catPart_113 )) 
res_110.recoverWith{ case p: ParseError => 
reset(pos0_109)
Failure( p  )
}
}


def EOF(): Try[PTree] = {
val pos_117 = mark
val res_118 = {
val pos_119 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError =>
reset(pos_119)
 Failure( p ~ ParseFailed("Expected any char",pos_119) )  
}
}
reset(pos_117)
if( res_118.isSuccess ) Failure(ParseFailed("Neglook failed",pos_117))
 else { Try(PEmpty) }
}


def WS(): Try[PTree] = {
var parts_120 = ArrayBuffer.empty[PTree]
var pos_121 = mark
var res_122 = {
val pos_123 = mark
val res_124 = for{
char_125 <- expect(' ','\t','\n','\r')
 } yield PLeaf(char_125.toString) 
res_124.recoverWith{ case p: ParseError =>
reset(pos_123)
 Failure( p ~ ParseFailed("Expected one of ' ','\t','\n','\r'",pos_123) ) 
}
}
res_122.recover{ _ => reset(pos_121) }
while(res_122.isSuccess){
parts_120 += res_122.get
pos_121 = mark
res_122 = {
val pos_126 = mark
val res_127 = for{
char_128 <- expect(' ','\t','\n','\r')
 } yield PLeaf(char_128.toString) 
res_127.recoverWith{ case p: ParseError =>
reset(pos_126)
 Failure( p ~ ParseFailed("Expected one of ' ','\t','\n','\r'",pos_126) ) 
}
}
res_122.recover{ _ => reset(pos_121) }
}
 Try(PBranch("WS",parts_120.toSeq)) 
}
}
