package PEG.PEGParser

import PEG.lexparse.{Lexer, Parser}
import PEG.data.implicits._
import PEG.data._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}


import PEG.generators.PEGActionGenerator
class GeneratedPEGActionParser(lexer: Lexer) extends Parser(lexer){


def Grammar(): Try[List[Definition]] = {
val pos_1 = mark
val res_2 = for{
_ <- Spacing()
ds <- {
val pos_5 = mark
val res_6 = for{
ast0_3 <- Definition()
asts_4 <- {
def asts_4_sub_10 = Definition()
var buf_7 = ArrayBuffer.empty[Definition]
var pos_8 = mark
var res_9 = asts_4_sub_10
res_9.recover{ _ => reset(pos_8) }
while(res_9.isSuccess){
buf_7 += res_9.get
pos_8 = mark
res_9 = asts_4_sub_10
res_9.recover{ _ => reset(pos_8) }
}
 Try( buf_7.toList )
}
} yield {ast0_3 :: asts_4}
res_6.recoverWith{ case p: ParseError[Char] => 
reset(pos_5)
Failure(p)
}
}
_ <- EndOfFile()
} yield {ds }
res_2.recoverWith{ case p: ParseError[Char] => 
reset(pos_1)
Failure(p)
}
}


def Definition(): Try[Definition] = {
val pos_11 = mark
val res_12 = for{
Var(id) <- Identifier()
memo <- {
val pos_13 = mark
STAR().recoverWith{ case err_14: ParseError[Char] =>
reset(pos_13)
 Try(PEmpty) 
}
}
_ <- LEFTARROW()
e <- Expression()
} yield {PEG.data.Definition(id,memo != PEmpty,e)
                }
res_12.recoverWith{ case p: ParseError[Char] => 
reset(pos_11)
Failure(p)
}
}


def Expression(): Try[PEGAst] = {
val pos_15 = mark
val res_16 = for{
s <- Sequence()
ss <- {
def ss_sub_20 = {
val pos_21 = mark
val res_22 = for{
_ <- SLASH()
s <- Sequence()
} yield {s}
res_22.recoverWith{ case p: ParseError[Char] => 
reset(pos_21)
Failure(p)
}
}
var buf_17 = ArrayBuffer.empty[PEGAst]
var pos_18 = mark
var res_19 = ss_sub_20
res_19.recover{ _ => reset(pos_18) }
while(res_19.isSuccess){
buf_17 += res_19.get
pos_18 = mark
res_19 = ss_sub_20
res_19.recover{ _ => reset(pos_18) }
}
 Try( buf_17.toList )
}
} yield {if(ss.isEmpty) s else Alt(s :: ss)
                }
res_16.recoverWith{ case p: ParseError[Char] => 
reset(pos_15)
Failure(p)
}
}


def Sequence(): Try[PEGAst] = {
val pos_23 = mark
val res_24 = for{
ps <- {
def ps_sub_28 = Prefix()
var buf_25 = ArrayBuffer.empty[PEGAst]
var pos_26 = mark
var res_27 = ps_sub_28
res_27.recover{ _ => reset(pos_26) }
while(res_27.isSuccess){
buf_25 += res_27.get
pos_26 = mark
res_27 = ps_sub_28
res_27.recover{ _ => reset(pos_26) }
}
 Try( buf_25.toList )
}
act <- {
val pos_30 = mark
val res_31 = {
{
val pos_33 = mark
SymAction().recoverWith{ case p: ParseError[Char] =>
reset(pos_33)
 Failure( p ~ ParseFailed("expected Var 'SymAction'",pos_33) ) 
}
}
.map{ case (arg_29) => 
Option(arg_29)
}
}
res_31.recoverWith{ case err_32: ParseError[Char] =>
reset(pos_30)
val res_34 = {
Try{Option.empty}
}
res_34.recoverWith{ case err_35: ParseError[Char] =>
reset(pos_30)
Failure(err_32 ~ err_35 ~  ParseFailed("",pos_30) )
}
}
}
} yield {val ast = if(ps.isEmpty) Empty else if(ps.length == 1) ps.head else Cat(ps)
                       if( act == Option.empty) ast
                       else Action(ast,act.get._1,act.get._2,act.get._3)
                 }
res_24.recoverWith{ case p: ParseError[Char] => 
reset(pos_23)
Failure(p)
}
}


def SymAction(): Try[(String,List[String],String)] = {
val pos_36 = mark
val res_37 = for{
_ <- CURRLYOPEN()
_type <- {
val pos_40 = mark
val res_41 = for{
ast0_38 <- {
val pos_42 = mark
val res_43 = for{
_ <- {
val pos_44 = mark
val res_45 = expect('|')
reset(pos_44)
if( res_45.isSuccess ) Failure(ParseFailed("Neglook failed",pos_44))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_43.recoverWith{ case p: ParseError[Char] => 
reset(pos_42)
Failure(p)
}
}
asts_39 <- {
def asts_39_sub_49 = {
val pos_50 = mark
val res_51 = for{
_ <- {
val pos_52 = mark
val res_53 = expect('|')
reset(pos_52)
if( res_53.isSuccess ) Failure(ParseFailed("Neglook failed",pos_52))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_51.recoverWith{ case p: ParseError[Char] => 
reset(pos_50)
Failure(p)
}
}
var buf_46 = ArrayBuffer.empty[Char]
var pos_47 = mark
var res_48 = asts_39_sub_49
res_48.recover{ _ => reset(pos_47) }
while(res_48.isSuccess){
buf_46 += res_48.get
pos_47 = mark
res_48 = asts_39_sub_49
res_48.recover{ _ => reset(pos_47) }
}
 Try( buf_46.toList )
}
} yield {ast0_38 :: asts_39}
res_41.recoverWith{ case p: ParseError[Char] => 
reset(pos_40)
Failure(p)
}
}
_ <- VERTBAR()
args <- {
val pos_56 = mark
val res_57 = for{
ast0_54 <- SymActionArg()
asts_55 <- {
def asts_55_sub_61 = SymActionArg()
var buf_58 = ArrayBuffer.empty[String]
var pos_59 = mark
var res_60 = asts_55_sub_61
res_60.recover{ _ => reset(pos_59) }
while(res_60.isSuccess){
buf_58 += res_60.get
pos_59 = mark
res_60 = asts_55_sub_61
res_60.recover{ _ => reset(pos_59) }
}
 Try( buf_58.toList )
}
} yield {ast0_54 :: asts_55}
res_57.recoverWith{ case p: ParseError[Char] => 
reset(pos_56)
Failure(p)
}
}
_ <- VERTBAR()
body <- {
val pos_64 = mark
val res_65 = for{
ast0_62 <- {
val pos_66 = mark
val res_67 = for{
_ <- {
val pos_68 = mark
val res_69 = expect('}')
reset(pos_68)
if( res_69.isSuccess ) Failure(ParseFailed("Neglook failed",pos_68))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_67.recoverWith{ case p: ParseError[Char] =>
reset(pos_66)
Failure(p)
}
}
asts_63 <- {
def asts_63_sub_73 = {
val pos_74 = mark
val res_75 = for{
_ <- {
val pos_76 = mark
val res_77 = expect('}')
reset(pos_76)
if( res_77.isSuccess ) Failure(ParseFailed("Neglook failed",pos_76))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_75.recoverWith{ case p: ParseError[Char] =>
reset(pos_74)
Failure(p)
}
}
var buf_70 = ArrayBuffer.empty[Char]
var pos_71 = mark
var res_72 = asts_63_sub_73
res_72.recover{ _ => reset(pos_71) }
while(res_72.isSuccess){
buf_70 += res_72.get
pos_71 = mark
res_72 = asts_63_sub_73
res_72.recover{ _ => reset(pos_71) }
}
 Try( buf_70.toList )
}
} yield {ast0_62 :: asts_63}
res_65.recoverWith{ case p: ParseError[Char] =>
reset(pos_64)
Failure(p)
}
}
_ <- CURRLYCLOSE()
} yield {(_type.mkString(""),args,body.mkString(""))
              }
res_37.recoverWith{ case p: ParseError[Char] =>
reset(pos_36)
Failure(p)
}
}


def SymActionArg(): Try[String] = {
val pos_78 = mark
val res_79 = for{
s <- {
val pos_82 = mark
val res_83 = for{
ast0_80 <- {
val pos_84 = mark
val res_85 = for{
_ <- {
val pos_86 = mark
val res_87 = expect('|',' ','\t','\n')
reset(pos_86)
if( res_87.isSuccess ) Failure(ParseFailed("Neglook failed",pos_86))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c }
res_85.recoverWith{ case p: ParseError[Char] =>
reset(pos_84)
Failure(p)
}
}
asts_81 <- {
def asts_81_sub_91 = {
val pos_92 = mark
val res_93 = for{
_ <- {
val pos_94 = mark
val res_95 = expect('|',' ','\t','\n')
reset(pos_94)
if( res_95.isSuccess ) Failure(ParseFailed("Neglook failed",pos_94))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c }
res_93.recoverWith{ case p: ParseError[Char] =>
reset(pos_92)
Failure(p)
}
}
var buf_88 = ArrayBuffer.empty[Char]
var pos_89 = mark
var res_90 = asts_81_sub_91
res_90.recover{ _ => reset(pos_89) }
while(res_90.isSuccess){
buf_88 += res_90.get
pos_89 = mark
res_90 = asts_81_sub_91
res_90.recover{ _ => reset(pos_89) }
}
 Try( buf_88.toList )
}
} yield {ast0_80 :: asts_81}
res_83.recoverWith{ case p: ParseError[Char] =>
reset(pos_82)
Failure(p)
}
}
_ <- Spacing()
} yield {s.mkString("") }
res_79.recoverWith{ case p: ParseError[Char] =>
reset(pos_78)
Failure(p)
}
}


def Prefix(): Try[PEGAst] = {
val pos_96 = mark
val res_97 = {
val pos_99 = mark
val res_100 = for{
_ <- AND()
s <- Suffix()
} yield {PosLook(s) }
res_100.recoverWith{ case p: ParseError[Char] =>
reset(pos_99)
Failure(p)
}
}
res_97.recoverWith{ case err_98: ParseError[Char] =>
reset(pos_96)
val res_101 = {
val pos_103 = mark
val res_104 = for{
_ <- NOT()
s <- Suffix()
} yield {NegLook(s) }
res_104.recoverWith{ case p: ParseError[Char] =>
reset(pos_103)
Failure(p)
}
}
res_101.recoverWith{ case err_102: ParseError[Char] =>
reset(pos_96)
Suffix().recoverWith{ case err_105: ParseError[Char] =>
reset(pos_96)
Failure(err_98 ~ err_102 ~ err_105 ~  ParseFailed("",pos_96) )
}
}
}
}


def Suffix(): Try[PEGAst] = {
val pos_106 = mark
val res_107 = {
val pos_109 = mark
val res_110 = for{
p <- Primary()
_ <- STAR()
} yield {Star(p)     }
res_110.recoverWith{ case p: ParseError[Char] =>
reset(pos_109)
Failure(p)
}
}
res_107.recoverWith{ case err_108: ParseError[Char] =>
reset(pos_106)
val res_111 = {
val pos_113 = mark
val res_114 = for{
p <- Primary()
_ <- PLUS()
} yield {Plus(p)     }
res_114.recoverWith{ case p: ParseError[Char] =>
reset(pos_113)
Failure(p)
}
}
res_111.recoverWith{ case err_112: ParseError[Char] =>
reset(pos_106)
val res_115 = {
val pos_117 = mark
val res_118 = for{
p <- Primary()
_ <- QUESTION()
} yield {Optional(p) }
res_118.recoverWith{ case p: ParseError[Char] =>
reset(pos_117)
Failure(p)
}
}
res_115.recoverWith{ case err_116: ParseError[Char] =>
reset(pos_106)
val res_119 = {
{
val pos_121 = mark
Primary().recoverWith{ case p: ParseError[Char] =>
reset(pos_121)
 Failure( p ~ ParseFailed("expected Var 'Primary'",pos_121) )
}
}
.map{ case (p) =>
p
}
}
res_119.recoverWith{ case err_120: ParseError[Char] =>
reset(pos_106)
Failure(err_108 ~ err_112 ~ err_116 ~ err_120 ~  ParseFailed("",pos_106) )
}
}
}
}
}


val cache_123 = mutable.HashMap.empty[Int,(Try[PEGAst],Int)]
def Primary(): Try[PEGAst] = {
def parser_122(): Try[PEGAst] = {
val pos_127 = mark
val res_128 = {
val pos_130 = mark
val res_131 = for{
id <- Identifier()
_ <- {
val pos_132 = mark
val res_133 = {
val pos0_134 = mark
val res_135 = for{
catPart_136 <- {
val pos_138 = mark
STAR().recoverWith{ case err_139: ParseError[Char] =>
reset(pos_138)
 Try(PEmpty)
}
}
catPart_137 <- LEFTARROW()
 }  yield PBranch("_",Seq( catPart_136,catPart_137 ))
res_135.recoverWith{ case p: ParseError[Char] =>
reset(pos0_134)
Failure( p  )
}
}
reset(pos_132)
if( res_133.isSuccess ) Failure(ParseFailed("Neglook failed",pos_132))
 else { Try(PEmpty) }
}
} yield {id }
res_131.recoverWith{ case p: ParseError[Char] =>
reset(pos_130)
Failure(p)
}
}
res_128.recoverWith{ case err_129: ParseError[Char] =>
reset(pos_127)
val res_140 = {
val pos_142 = mark
val res_143 = for{
_ <- OPEN()
e <- Expression()
_ <- CLOSE()
} yield {e}
res_143.recoverWith{ case p: ParseError[Char] =>
reset(pos_142)
Failure(p)
}
}
res_140.recoverWith{ case err_141: ParseError[Char] =>
reset(pos_127)
val res_144 = {
{
val pos_146 = mark
DOT().recoverWith{ case p: ParseError[Char] =>
reset(pos_146)
 Failure( p ~ ParseFailed("expected Var 'DOT'",pos_146) )
}
}
.map{ case (_) =>
Any
}
}
res_144.recoverWith{ case err_145: ParseError[Char] =>
reset(pos_127)
Literal().recoverWith{ case err_147: ParseError[Char] =>
reset(pos_127)
CharClass().recoverWith{ case err_148: ParseError[Char] =>
reset(pos_127)
Failure(err_129 ~ err_141 ~ err_145 ~ err_147 ~ err_148 ~  ParseFailed("",pos_127) )
}
}
}
}
}
}
if(!cache_123.contains(mark)){
val init_125 = mark
cache_123(init_125) = parser_122() -> mark
reset(init_125)
}
val (res_124,pos_126) = cache_123(mark)
reset(pos_126)
res_124
}


def Identifier(): Try[PEGAst] = {
val pos_149 = mark
val res_150 = for{
s <- IdentStart()
p <- {
def p_sub_154 = IdentPart()
var buf_151 = ArrayBuffer.empty[Char]
var pos_152 = mark
var res_153 = p_sub_154
res_153.recover{ _ => reset(pos_152) }
while(res_153.isSuccess){
buf_151 += res_153.get
pos_152 = mark
res_153 = p_sub_154
res_153.recover{ _ => reset(pos_152) }
}
 Try( buf_151.toList )
}
_ <- Spacing()
} yield {Var((s::p).mkString("")) }
res_150.recoverWith{ case p: ParseError[Char] =>
reset(pos_149)
Failure(p)
}
}


def IdentStart(): Try[Char] = {
{
val pos_155 = mark
expect('F','D','X','J','P','Y','K','H','_','L','I','M','R','V','E','A','S','W','O','B','T','N','Q','U','Z','C','G')
.map{ char_156 => PLeaf(char_156.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_155)
 Failure( p ~ ParseFailed("Expected one of 'F','D','X','J','P','Y','K','H','_','L','I','M','R','V','E','A','S','W','O','B','T','N','Q','U','Z','C','G'",pos_155) )
}
}
.map{ case (PLeaf(c)) =>
c(0)
}
}


def IdentPart(): Try[Char] = {
val pos_157 = mark
IdentStart().recoverWith{ case err_158: ParseError[Char] =>
reset(pos_157)
val res_159 = {
{
val pos_161 = mark
expect('a','u','v','f','x','s','d','e','t','b','n','y','k','h','z','r','g','l','c','q','m','i','j','p','w','o')
.map{ char_162 => PLeaf(char_162.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_161)
 Failure( p ~ ParseFailed("Expected one of 'a','u','v','f','x','s','d','e','t','b','n','y','k','h','z','r','g','l','c','q','m','i','j','p','w','o'",pos_161) )
}
}
.map{ case (PLeaf(c)) =>
c(0)
}
}
res_159.recoverWith{ case err_160: ParseError[Char] =>
reset(pos_157)
val res_163 = {
{
val pos_165 = mark
expect('6','9','2','8','4','0','5','1','7','3')
.map{ char_166 => PLeaf(char_166.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_165)
 Failure( p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'",pos_165) )
}
}
.map{ case (PLeaf(c)) =>
c(0)
}
}
res_163.recoverWith{ case err_164: ParseError[Char] =>
reset(pos_157)
Failure(err_158 ~ err_160 ~ err_164 ~  ParseFailed("",pos_157) )
}
}
}
}


def Literal(): Try[PEGAst] = {
val pos_167 = mark
val res_168 = {
val pos_170 = mark
val res_171 = for{
_ <- expect('\'').map{ char_172 => PLeaf(char_172.toString) }
l <- {
def l_sub_176 = {
val pos_177 = mark
val res_178 = for{
_ <- {
val pos_179 = mark
val res_180 = expect('\'')
reset(pos_179)
if( res_180.isSuccess ) Failure(ParseFailed("Neglook failed",pos_179))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_178.recoverWith{ case p: ParseError[Char] =>
reset(pos_177)
Failure(p)
}
}
var buf_173 = ArrayBuffer.empty[Char]
var pos_174 = mark
var res_175 = l_sub_176
res_175.recover{ _ => reset(pos_174) }
while(res_175.isSuccess){
buf_173 += res_175.get
pos_174 = mark
res_175 = l_sub_176
res_175.recover{ _ => reset(pos_174) }
}
 Try( buf_173.toList )
}
_ <- expect('\'').map{ char_181 => PLeaf(char_181.toString) }
_ <- Spacing()
} yield {Lit(l) }
res_171.recoverWith{ case p: ParseError[Char] =>
reset(pos_170)
Failure(p)
}
}
res_168.recoverWith{ case err_169: ParseError[Char] =>
reset(pos_167)
val res_182 = {
val pos_184 = mark
val res_185 = for{
_ <- expect('"').map{ char_186 => PLeaf(char_186.toString) }
l <- {
def l_sub_190 = {
val pos_191 = mark
val res_192 = for{
_ <- {
val pos_193 = mark
val res_194 = expect('"')
reset(pos_193)
if( res_194.isSuccess ) Failure(ParseFailed("Neglook failed",pos_193))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_192.recoverWith{ case p: ParseError[Char] =>
reset(pos_191)
Failure(p)
}
}
var buf_187 = ArrayBuffer.empty[Char]
var pos_188 = mark
var res_189 = l_sub_190
res_189.recover{ _ => reset(pos_188) }
while(res_189.isSuccess){
buf_187 += res_189.get
pos_188 = mark
res_189 = l_sub_190
res_189.recover{ _ => reset(pos_188) }
}
 Try( buf_187.toList )
}
_ <- expect('"').map{ char_195 => PLeaf(char_195.toString) }
_ <- Spacing()
} yield {Lit(l) }
res_185.recoverWith{ case p: ParseError[Char] =>
reset(pos_184)
Failure(p)
}
}
res_182.recoverWith{ case err_183: ParseError[Char] =>
reset(pos_167)
val res_196 = {
val pos_198 = mark
val res_199 = for{
_ <- expect('`').map{ char_200 => PLeaf(char_200.toString) }
l <- {
def l_sub_204 = {
val pos_205 = mark
val res_206 = for{
_ <- {
val pos_207 = mark
val res_208 = expect('`')
reset(pos_207)
if( res_208.isSuccess ) Failure(ParseFailed("Neglook failed",pos_207))
 else { Try(PEmpty) }
}
c <- Char()
} yield {c}
res_206.recoverWith{ case p: ParseError[Char] =>
reset(pos_205)
Failure(p)
}
}
var buf_201 = ArrayBuffer.empty[Char]
var pos_202 = mark
var res_203 = l_sub_204
res_203.recover{ _ => reset(pos_202) }
while(res_203.isSuccess){
buf_201 += res_203.get
pos_202 = mark
res_203 = l_sub_204
res_203.recover{ _ => reset(pos_202) }
}
 Try( buf_201.toList )
}
_ <- expect('`').map{ char_209 => PLeaf(char_209.toString) }
_ <- Spacing()
} yield {Lit(l) }
res_199.recoverWith{ case p: ParseError[Char] =>
reset(pos_198)
Failure(p)
}
}
res_196.recoverWith{ case err_197: ParseError[Char] =>
reset(pos_167)
Failure(err_169 ~ err_183 ~ err_197 ~  ParseFailed("",pos_167) )
}
}
}
}


def CharClass(): Try[PEGAst] = {
val pos_210 = mark
val res_211 = for{
_ <- expect('[').map{ char_212 => PLeaf(char_212.toString) }
cs <- {
def cs_sub_216 = ClassPart()
var buf_213 = ArrayBuffer.empty[Set[Char]]
var pos_214 = mark
var res_215 = cs_sub_216
res_215.recover{ _ => reset(pos_214) }
while(res_215.isSuccess){
buf_213 += res_215.get
pos_214 = mark
res_215 = cs_sub_216
res_215.recover{ _ => reset(pos_214) }
}
 Try( buf_213.toList )
}
_ <- expect(']').map{ char_217 => PLeaf(char_217.toString) }
_ <- Spacing()
} yield {Class(cs.fold(Set.empty)(_ union _)) }
res_211.recoverWith{ case p: ParseError[Char] =>
reset(pos_210)
Failure(p)
}
}


def ClassPart(): Try[Set[Char]] = {
val pos_218 = mark
val res_219 = for{
_ <- {
val pos_220 = mark
val res_221 = expect(']')
reset(pos_220)
if( res_221.isSuccess ) Failure(ParseFailed("Neglook failed",pos_220))
 else { Try(PEmpty) }
}
s <- Range()
} yield {s}
res_219.recoverWith{ case p: ParseError[Char] =>
reset(pos_218)
Failure(p)
}
}


def Range(): Try[Set[Char]] = {
val pos_222 = mark
val res_223 = {
val pos_225 = mark
val res_226 = for{
c0 <- Char()
_ <- expect('-').map{ char_227 => PLeaf(char_227.toString) }
c1 <- Char()
} yield {c0.to(c1).toSet }
res_226.recoverWith{ case p: ParseError[Char] =>
reset(pos_225)
Failure(p)
}
}
res_223.recoverWith{ case err_224: ParseError[Char] =>
reset(pos_222)
val res_228 = {
{
val pos_230 = mark
Char().recoverWith{ case p: ParseError[Char] =>
reset(pos_230)
 Failure( p ~ ParseFailed("expected Var 'Char'",pos_230) )
}
}
.map{ case (c) =>
Set(c)
}
}
res_228.recoverWith{ case err_229: ParseError[Char] =>
reset(pos_222)
Failure(err_224 ~ err_229 ~  ParseFailed("",pos_222) )
}
}
}


def Char(): Try[Char] = {
val pos_231 = mark
val res_232 = {
val pos_234 = mark
val res_235 = for{
_ <- expect('\\').map{ char_236 => PLeaf(char_236.toString) }
PLeaf(c) <- expect('n','[','\\','\'','r',']','t','"').map{ char_237 => PLeaf(char_237.toString) }
} yield {PEGActionGenerator.escapeChar(c)  }
res_235.recoverWith{ case p: ParseError[Char] =>
reset(pos_234)
Failure(p)
}
}
res_232.recoverWith{ case err_233: ParseError[Char] =>
reset(pos_231)
val res_238 = {
val pos_240 = mark
val res_241 = for{
_ <- {
val pos_242 = mark
val res_243 = {
val pos_244= mark
val res_245 = for{
char_part_246 <- expect('\\')
 } yield PBranch("Lit",Seq( PLeaf(char_part_246.toString) ))
res_245.recoverWith{ case p: ParseError[Char] =>
reset(pos_244)
 Failure( p ~ ParseFailed("expected '\\'",pos_244) )
}
}
reset(pos_242)
if( res_243.isSuccess ) Failure(ParseFailed("Neglook failed",pos_242))
 else { Try(PEmpty) }
}
PLeaf(c) <- any.map{ char_247 => PLeaf(char_247.toString)}
} yield {c(0) }
res_241.recoverWith{ case p: ParseError[Char] =>
reset(pos_240)
Failure(p)
}
}
res_238.recoverWith{ case err_239: ParseError[Char] =>
reset(pos_231)
Failure(err_233 ~ err_239 ~  ParseFailed("",pos_231) )
}
}
}


def LEFTARROW(): Try[PTree] = {
val pos0_248 = mark
val res_249 = for{
_ <- expect('<')
_ <- expect('-')
 catPart_250 <- Try( PBranch("Lit",Seq(PLeaf('<'.toString),PLeaf('-'.toString))) )
catPart_251 <- Spacing()
 }  yield PBranch("LEFTARROW",Seq( catPart_250,catPart_251 ))
res_249.recoverWith{ case p: ParseError[Char] =>
reset(pos0_248)
Failure( p  )
}
}


def OPEN(): Try[PTree] = {
val pos0_252 = mark
val res_253 = for{
catPart_254 <- expect('(').map{ char_256 => PLeaf(char_256.toString) }
catPart_255 <- Spacing()
 }  yield PBranch("OPEN",Seq( catPart_254,catPart_255 ))
res_253.recoverWith{ case p: ParseError[Char] =>
reset(pos0_252)
Failure( p  )
}
}


def CLOSE(): Try[PTree] = {
val pos0_257 = mark
val res_258 = for{
catPart_259 <- expect(')').map{ char_261 => PLeaf(char_261.toString) }
catPart_260 <- Spacing()
 }  yield PBranch("CLOSE",Seq( catPart_259,catPart_260 ))
res_258.recoverWith{ case p: ParseError[Char] =>
reset(pos0_257)
Failure( p  )
}
}


def DOT(): Try[PTree] = {
val pos0_262 = mark
val res_263 = for{
catPart_264 <- expect('.').map{ char_266 => PLeaf(char_266.toString) }
catPart_265 <- Spacing()
 }  yield PBranch("DOT",Seq( catPart_264,catPart_265 ))
res_263.recoverWith{ case p: ParseError[Char] =>
reset(pos0_262)
Failure( p  )
}
}


def STAR(): Try[PTree] = {
val pos0_267 = mark
val res_268 = for{
catPart_269 <- expect('*').map{ char_271 => PLeaf(char_271.toString) }
catPart_270 <- Spacing()
 }  yield PBranch("STAR",Seq( catPart_269,catPart_270 ))
res_268.recoverWith{ case p: ParseError[Char] =>
reset(pos0_267)
Failure( p  )
}
}


def PLUS(): Try[PTree] = {
val pos0_272 = mark
val res_273 = for{
catPart_274 <- expect('+').map{ char_276 => PLeaf(char_276.toString) }
catPart_275 <- Spacing()
 }  yield PBranch("PLUS",Seq( catPart_274,catPart_275 ))
res_273.recoverWith{ case p: ParseError[Char] =>
reset(pos0_272)
Failure( p  )
}
}


def QUESTION(): Try[PTree] = {
val pos0_277 = mark
val res_278 = for{
catPart_279 <- expect('?').map{ char_281 => PLeaf(char_281.toString) }
catPart_280 <- Spacing()
 }  yield PBranch("QUESTION",Seq( catPart_279,catPart_280 ))
res_278.recoverWith{ case p: ParseError[Char] =>
reset(pos0_277)
Failure( p  )
}
}


def AND(): Try[PTree] = {
val pos0_282 = mark
val res_283 = for{
catPart_284 <- expect('&').map{ char_286 => PLeaf(char_286.toString) }
catPart_285 <- Spacing()
 }  yield PBranch("AND",Seq( catPart_284,catPart_285 ))
res_283.recoverWith{ case p: ParseError[Char] =>
reset(pos0_282)
Failure( p  )
}
}


def NOT(): Try[PTree] = {
val pos0_287 = mark
val res_288 = for{
catPart_289 <- expect('!').map{ char_291 => PLeaf(char_291.toString) }
catPart_290 <- Spacing()
 }  yield PBranch("NOT",Seq( catPart_289,catPart_290 ))
res_288.recoverWith{ case p: ParseError[Char] =>
reset(pos0_287)
Failure( p  )
}
}


def SLASH(): Try[PTree] = {
val pos0_292 = mark
val res_293 = for{
catPart_294 <- expect('/').map{ char_296 => PLeaf(char_296.toString) }
catPart_295 <- Spacing()
 }  yield PBranch("SLASH",Seq( catPart_294,catPart_295 ))
res_293.recoverWith{ case p: ParseError[Char] =>
reset(pos0_292)
Failure( p  )
}
}


def CURRLYOPEN(): Try[PTree] = {
val pos0_297 = mark
val res_298 = for{
catPart_299 <- expect('{').map{ char_301 => PLeaf(char_301.toString) }
catPart_300 <- Spacing()
 }  yield PBranch("CURRLYOPEN",Seq( catPart_299,catPart_300 ))
res_298.recoverWith{ case p: ParseError[Char] =>
reset(pos0_297)
Failure( p  )
}
}


def CURRLYCLOSE(): Try[PTree] = {
val pos0_302 = mark
val res_303 = for{
catPart_304 <- expect('}').map{ char_306 => PLeaf(char_306.toString) }
catPart_305 <- Spacing()
 }  yield PBranch("CURRLYCLOSE",Seq( catPart_304,catPart_305 ))
res_303.recoverWith{ case p: ParseError[Char] =>
reset(pos0_302)
Failure( p  )
}
}


def VERTBAR(): Try[PTree] = {
val pos0_307 = mark
val res_308 = for{
catPart_309 <- expect('|').map{ char_311 => PLeaf(char_311.toString) }
catPart_310 <- Spacing()
 }  yield PBranch("VERTBAR",Seq( catPart_309,catPart_310 ))
res_308.recoverWith{ case p: ParseError[Char] =>
reset(pos0_307)
Failure( p  )
}
}


def COMMA(): Try[PTree] = {
val pos0_312 = mark
val res_313 = for{
catPart_314 <- expect(',').map{ char_316 => PLeaf(char_316.toString) }
catPart_315 <- Spacing()
 }  yield PBranch("COMMA",Seq( catPart_314,catPart_315 ))
res_313.recoverWith{ case p: ParseError[Char] =>
reset(pos0_312)
Failure( p  )
}
}


def Spacing(): Try[PTree] = {
def Spacing_sub_320 = {
val pos_321 = mark
Space().recoverWith{ case err_322: ParseError[Char] =>
reset(pos_321)
Comment().recoverWith{ case err_323: ParseError[Char] =>
reset(pos_321)
Failure(err_322 ~ err_323 ~  ParseFailed("",pos_321) )
}
}
}
var buf_317 = ArrayBuffer.empty[PTree]
var pos_318 = mark
var res_319 = Spacing_sub_320
res_319.recover{ _ => reset(pos_318) }
while(res_319.isSuccess){
buf_317 += res_319.get
pos_318 = mark
res_319 = Spacing_sub_320
res_319.recover{ _ => reset(pos_318) }
}
 Try(PBranch("Spacing",buf_317.toSeq))
}


def Comment(): Try[PTree] = {
val pos0_324 = mark
val res_325 = for{
catPart_326 <- expect('#').map{ char_329 => PLeaf(char_329.toString) }
catPart_327 <- {
def catPart_327_sub_333 = {
val pos0_334 = mark
val res_335 = for{
catPart_336 <- {
val pos_338 = mark
val res_339 = EndOfLine()
reset(pos_338)
if( res_339.isSuccess ) Failure(ParseFailed("Neglook failed",pos_338))
 else { Try(PEmpty) }
}
catPart_337 <- any.map{ char_340 => PLeaf(char_340.toString)}
 }  yield PBranch("catPart_327",Seq( catPart_336,catPart_337 ))
res_335.recoverWith{ case p: ParseError[Char] =>
reset(pos0_334)
Failure( p  )
}
}
var buf_330 = ArrayBuffer.empty[PTree]
var pos_331 = mark
var res_332 = catPart_327_sub_333
res_332.recover{ _ => reset(pos_331) }
while(res_332.isSuccess){
buf_330 += res_332.get
pos_331 = mark
res_332 = catPart_327_sub_333
res_332.recover{ _ => reset(pos_331) }
}
 Try(PBranch("catPart_327",buf_330.toSeq))
}
catPart_328 <- EndOfLine()
 }  yield PBranch("Comment",Seq( catPart_326,catPart_327,catPart_328 ))
res_325.recoverWith{ case p: ParseError[Char] =>
reset(pos0_324)
Failure( p  )
}
}


def Space(): Try[PTree] = {
val pos_341 = mark
expect(' ').map{ char_342 => PLeaf(char_342.toString) }
.recoverWith{ case err_343: ParseError[Char] =>
reset(pos_341)
expect('\t').map{ char_344 => PLeaf(char_344.toString) }
.recoverWith{ case err_345: ParseError[Char] =>
reset(pos_341)
EndOfLine().recoverWith{ case err_346: ParseError[Char] =>
reset(pos_341)
Failure(err_343 ~ err_345 ~ err_346 ~  ParseFailed("",pos_341) )
}
}
}
}


def EndOfLine(): Try[PTree] = {
val pos_347 = mark
val res_348 = {
val pos_350= mark
val res_351 = for{
char_part_352 <- expect('\r')
char_part_353 <- expect('\n')
 } yield PBranch("Lit",Seq( PLeaf(char_part_352.toString) , PLeaf(char_part_353.toString) ))
res_351.recoverWith{ case p: ParseError[Char] =>
reset(pos_350)
 Failure( p ~ ParseFailed("expected '\r','\n'",pos_350) )
}
}
res_348.recoverWith{ case err_349: ParseError[Char] =>
reset(pos_347)
expect('\n').map{ char_354 => PLeaf(char_354.toString) }
.recoverWith{ case err_355: ParseError[Char] =>
reset(pos_347)
expect('\r').map{ char_356 => PLeaf(char_356.toString) }
.recoverWith{ case err_357: ParseError[Char] =>
reset(pos_347)
Failure(err_349 ~ err_355 ~ err_357 ~  ParseFailed("",pos_347) )
}
}
}
}


def EndOfFile(): Try[PTree] = {
val pos_358 = mark
val res_359 = {
val pos_360 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError[Char] =>
reset(pos_360)
 Failure( p ~ ParseFailed("Expected any char",pos_360) )
}
}
reset(pos_358)
if( res_359.isSuccess ) Failure(ParseFailed("Neglook failed",pos_358))
 else { Try(PEmpty) }
}
}
