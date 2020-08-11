package PEG.PEGParser

import PEG.lexparse.{Lexer, Parser}
import PEG.data.implicits._
import PEG.data._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}


class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer){


def Grammar(): Try[PTree] = {
val pos0_1 = mark
val res_2 = for{
catPart_3 <- Spacing()
catPart_4 <- {
val pos0_6 = mark
val res_7 = for{
catPart_8 <- Definition()
catPart_9 <- {
def catPart_9_sub_13 = Definition()
var buf_10 = ArrayBuffer.empty[PTree]
var pos_11 = mark
var res_12 = catPart_9_sub_13
res_12.recover{ _ => reset(pos_11) }
while(res_12.isSuccess){
buf_10 += res_12.get
pos_11 = mark
res_12 = catPart_9_sub_13
res_12.recover{ _ => reset(pos_11) }
}
 Try(PBranch("catPart_9",buf_10.toSeq)) 
}
 }  yield PBranch("catPart_4",Seq( catPart_8,catPart_9 )) 
res_7.recoverWith{ case p: ParseError[Char] => 
reset(pos0_6)
Failure( p  )
}
}
catPart_5 <- EndOfFile()
 }  yield PBranch("Grammar",Seq( catPart_3,catPart_4,catPart_5 )) 
res_2.recoverWith{ case p: ParseError[Char] => 
reset(pos0_1)
Failure( p  )
}
}


def Definition(): Try[PTree] = {
val pos0_14 = mark
val res_15 = for{
catPart_16 <- Identifier()
catPart_17 <- {
val pos_20 = mark
STAR().recoverWith{ case err_21: ParseError[Char] =>
reset(pos_20)
 Try(PEmpty) 
}
}
catPart_18 <- LEFTARROW()
catPart_19 <- Expression()
 }  yield PBranch("Definition",Seq( catPart_16,catPart_17,catPart_18,catPart_19 )) 
res_15.recoverWith{ case p: ParseError[Char] => 
reset(pos0_14)
Failure( p  )
}
}


def Expression(): Try[PTree] = {
val pos0_22 = mark
val res_23 = for{
catPart_24 <- Sequence()
catPart_25 <- {
def catPart_25_sub_29 = {
val pos0_30 = mark
val res_31 = for{
catPart_32 <- SLASH()
catPart_33 <- Sequence()
 }  yield PBranch("catPart_25",Seq( catPart_32,catPart_33 )) 
res_31.recoverWith{ case p: ParseError[Char] => 
reset(pos0_30)
Failure( p  )
}
}
var buf_26 = ArrayBuffer.empty[PTree]
var pos_27 = mark
var res_28 = catPart_25_sub_29
res_28.recover{ _ => reset(pos_27) }
while(res_28.isSuccess){
buf_26 += res_28.get
pos_27 = mark
res_28 = catPart_25_sub_29
res_28.recover{ _ => reset(pos_27) }
}
 Try(PBranch("catPart_25",buf_26.toSeq)) 
}
 }  yield PBranch("Expression",Seq( catPart_24,catPart_25 )) 
res_23.recoverWith{ case p: ParseError[Char] => 
reset(pos0_22)
Failure( p  )
}
}


def Sequence(): Try[PTree] = {
val pos0_34 = mark
val res_35 = for{
catPart_36 <- {
def catPart_36_sub_41 = Prefix()
var buf_38 = ArrayBuffer.empty[PTree]
var pos_39 = mark
var res_40 = catPart_36_sub_41
res_40.recover{ _ => reset(pos_39) }
while(res_40.isSuccess){
buf_38 += res_40.get
pos_39 = mark
res_40 = catPart_36_sub_41
res_40.recover{ _ => reset(pos_39) }
}
 Try(PBranch("catPart_36",buf_38.toSeq)) 
}
catPart_37 <- {
val pos_42 = mark
Action().recoverWith{ case err_43: ParseError[Char] =>
reset(pos_42)
 Try(PEmpty) 
}
}
 }  yield PBranch("Sequence",Seq( catPart_36,catPart_37 )) 
res_35.recoverWith{ case p: ParseError[Char] => 
reset(pos0_34)
Failure( p  )
}
}


def Prefix(): Try[PTree] = {
val pos0_44 = mark
val res_45 = for{
catPart_46 <- {
val pos_48 = mark
val res_49 = {
val pos_51 = mark
AND().recoverWith{ case err_52: ParseError[Char] =>
reset(pos_51)
NOT().recoverWith{ case err_53: ParseError[Char] =>
reset(pos_51)
Failure(err_52 ~ err_53 ~  ParseFailed("",pos_51) )
}
}
}
res_49.recoverWith{ case err_50: ParseError[Char] =>
reset(pos_48)
 Try(PEmpty) 
}
}
catPart_47 <- Suffix()
 }  yield PBranch("Prefix",Seq( catPart_46,catPart_47 )) 
res_45.recoverWith{ case p: ParseError[Char] => 
reset(pos0_44)
Failure( p  )
}
}


def Suffix(): Try[PTree] = {
val pos0_54 = mark
val res_55 = for{
catPart_56 <- Primary()
catPart_57 <- {
val pos_58 = mark
val res_59 = {
val pos_61 = mark
QUESTION().recoverWith{ case err_62: ParseError[Char] =>
reset(pos_61)
STAR().recoverWith{ case err_63: ParseError[Char] =>
reset(pos_61)
PLUS().recoverWith{ case err_64: ParseError[Char] =>
reset(pos_61)
Failure(err_62 ~ err_63 ~ err_64 ~  ParseFailed("",pos_61) )
}
}
}
}
res_59.recoverWith{ case err_60: ParseError[Char] =>
reset(pos_58)
 Try(PEmpty) 
}
}
 }  yield PBranch("Suffix",Seq( catPart_56,catPart_57 )) 
res_55.recoverWith{ case p: ParseError[Char] => 
reset(pos0_54)
Failure( p  )
}
}


val cache_66 = mutable.HashMap.empty[Int,(Try[PTree],Int)]
def Primary(): Try[PTree] = {
def parser_65(): Try[PTree] = {
val pos_70 = mark
val res_71 = {
val pos0_73 = mark
val res_74 = for{
catPart_75 <- Identifier()
catPart_76 <- {
val pos_77 = mark
val res_78 = {
val pos0_79 = mark
val res_80 = for{
catPart_81 <- {
val pos_83 = mark
STAR().recoverWith{ case err_84: ParseError[Char] =>
reset(pos_83)
 Try(PEmpty) 
}
}
catPart_82 <- LEFTARROW()
 }  yield PBranch("catPart_76",Seq( catPart_81,catPart_82 )) 
res_80.recoverWith{ case p: ParseError[Char] => 
reset(pos0_79)
Failure( p  )
}
}
reset(pos_77)
if( res_78.isSuccess ) Failure(ParseFailed("Neglook failed",pos_77))
 else { Try(PEmpty) }
}
 }  yield PBranch("Primary",Seq( catPart_75,catPart_76 )) 
res_74.recoverWith{ case p: ParseError[Char] => 
reset(pos0_73)
Failure( p  )
}
}
res_71.recoverWith{ case err_72: ParseError[Char] =>
reset(pos_70)
val res_85 = {
val pos0_87 = mark
val res_88 = for{
catPart_89 <- OPEN()
catPart_90 <- Expression()
catPart_91 <- CLOSE()
 }  yield PBranch("Primary",Seq( catPart_89,catPart_90,catPart_91 )) 
res_88.recoverWith{ case p: ParseError[Char] => 
reset(pos0_87)
Failure( p  )
}
}
res_85.recoverWith{ case err_86: ParseError[Char] =>
reset(pos_70)
Literal().recoverWith{ case err_92: ParseError[Char] =>
reset(pos_70)
Class().recoverWith{ case err_93: ParseError[Char] =>
reset(pos_70)
DOT().recoverWith{ case err_94: ParseError[Char] =>
reset(pos_70)
Failure(err_72 ~ err_86 ~ err_92 ~ err_93 ~ err_94 ~  ParseFailed("",pos_70) )
}
}
}
}
}
}
if(!cache_66.contains(mark)){
val init_68 = mark
cache_66(init_68) = parser_65() -> mark
reset(init_68)
}
val (res_67,pos_69) = cache_66(mark)
reset(pos_69)
res_67
}


def Identifier(): Try[PTree] = {
val pos0_95 = mark
val res_96 = for{
catPart_97 <- IdentStart()
catPart_98 <- {
def catPart_98_sub_103 = IdentCont()
var buf_100 = ArrayBuffer.empty[PTree]
var pos_101 = mark
var res_102 = catPart_98_sub_103
res_102.recover{ _ => reset(pos_101) }
while(res_102.isSuccess){
buf_100 += res_102.get
pos_101 = mark
res_102 = catPart_98_sub_103
res_102.recover{ _ => reset(pos_101) }
}
 Try(PBranch("catPart_98",buf_100.toSeq)) 
}
catPart_99 <- Spacing()
 }  yield PBranch("Identifier",Seq( catPart_97,catPart_98,catPart_99 )) 
res_96.recoverWith{ case p: ParseError[Char] => 
reset(pos0_95)
Failure( p  )
}
}


def IdentStart(): Try[PTree] = {
val pos_104 = mark
expect('v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o')
.map{ char_105 => PLeaf(char_105.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_104)
 Failure( p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'",pos_104) ) 
}
}


def IdentCont(): Try[PTree] = {
val pos_106 = mark
IdentStart().recoverWith{ case err_107: ParseError[Char] =>
reset(pos_106)
expect('6','9','2','8','4','0','5','1','7','3')
.map{ char_108 => PLeaf(char_108.toString)}
.recoverWith{ case err_109: ParseError[Char] =>
reset(pos_106)
Failure(err_107 ~ err_109 ~  ParseFailed("",pos_106) )
}
}
}


def Action(): Try[PTree] = {
val pos0_110 = mark
val res_111 = for{
catPart_112 <- OPENCURLY()
catPart_113 <- {
val pos0_119 = mark
val res_120 = for{
catPart_121 <- {
val pos0_123 = mark
val res_124 = for{
catPart_125 <- {
val pos_127 = mark
val res_128 = expect('|','}')
reset(pos_127)
if( res_128.isSuccess ) Failure(ParseFailed("Neglook failed",pos_127))
 else { Try(PEmpty) }
}
catPart_126 <- any.map{ char_129 => PLeaf(char_129.toString)}
 }  yield PBranch("catPart_121",Seq( catPart_125,catPart_126 )) 
res_124.recoverWith{ case p: ParseError[Char] => 
reset(pos0_123)
Failure( p  )
}
}
catPart_122 <- {
def catPart_122_sub_133 = {
val pos0_134 = mark
val res_135 = for{
catPart_136 <- {
val pos_138 = mark
val res_139 = expect('|','}')
reset(pos_138)
if( res_139.isSuccess ) Failure(ParseFailed("Neglook failed",pos_138))
 else { Try(PEmpty) }
}
catPart_137 <- any.map{ char_140 => PLeaf(char_140.toString)}
 }  yield PBranch("catPart_122",Seq( catPart_136,catPart_137 )) 
res_135.recoverWith{ case p: ParseError[Char] => 
reset(pos0_134)
Failure( p  )
}
}
var buf_130 = ArrayBuffer.empty[PTree]
var pos_131 = mark
var res_132 = catPart_122_sub_133
res_132.recover{ _ => reset(pos_131) }
while(res_132.isSuccess){
buf_130 += res_132.get
pos_131 = mark
res_132 = catPart_122_sub_133
res_132.recover{ _ => reset(pos_131) }
}
 Try(PBranch("catPart_122",buf_130.toSeq)) 
}
 }  yield PBranch("catPart_113",Seq( catPart_121,catPart_122 )) 
res_120.recoverWith{ case p: ParseError[Char] => 
reset(pos0_119)
Failure( p  )
}
}
catPart_114 <- VERTBAR()
catPart_115 <- {
def catPart_115_sub_144 = ActionIdent()
var buf_141 = ArrayBuffer.empty[PTree]
var pos_142 = mark
var res_143 = catPart_115_sub_144
res_143.recover{ _ => reset(pos_142) }
while(res_143.isSuccess){
buf_141 += res_143.get
pos_142 = mark
res_143 = catPart_115_sub_144
res_143.recover{ _ => reset(pos_142) }
}
 Try(PBranch("catPart_115",buf_141.toSeq)) 
}
catPart_116 <- VERTBAR()
catPart_117 <- {
def catPart_117_sub_148 = {
val pos0_149 = mark
val res_150 = for{
catPart_151 <- {
val pos_153 = mark
val res_154 = expect('}')
reset(pos_153)
if( res_154.isSuccess ) Failure(ParseFailed("Neglook failed",pos_153))
 else { Try(PEmpty) }
}
catPart_152 <- any.map{ char_155 => PLeaf(char_155.toString)}
 }  yield PBranch("catPart_117",Seq( catPart_151,catPart_152 )) 
res_150.recoverWith{ case p: ParseError[Char] => 
reset(pos0_149)
Failure( p  )
}
}
var buf_145 = ArrayBuffer.empty[PTree]
var pos_146 = mark
var res_147 = catPart_117_sub_148
res_147.recover{ _ => reset(pos_146) }
while(res_147.isSuccess){
buf_145 += res_147.get
pos_146 = mark
res_147 = catPart_117_sub_148
res_147.recover{ _ => reset(pos_146) }
}
 Try(PBranch("catPart_117",buf_145.toSeq)) 
}
catPart_118 <- CLOSECURLY()
 }  yield PBranch("Action",Seq( catPart_112,catPart_113,catPart_114,catPart_115,catPart_116,catPart_117,catPart_118 )) 
res_111.recoverWith{ case p: ParseError[Char] => 
reset(pos0_110)
Failure( p  )
}
}


def ActionIdent(): Try[PTree] = {
val pos_156 = mark
val res_157 = {
val pos0_159 = mark
val res_160 = for{
catPart_161 <- Identifier()
catPart_162 <- OPEN()
catPart_163 <- ActionIdent()
catPart_164 <- {
def catPart_164_sub_169 = {
val pos0_170 = mark
val res_171 = for{
catPart_172 <- COMMA()
catPart_173 <- ActionIdent()
 }  yield PBranch("catPart_164",Seq( catPart_172,catPart_173 )) 
res_171.recoverWith{ case p: ParseError[Char] => 
reset(pos0_170)
Failure( p  )
}
}
var buf_166 = ArrayBuffer.empty[PTree]
var pos_167 = mark
var res_168 = catPart_164_sub_169
res_168.recover{ _ => reset(pos_167) }
while(res_168.isSuccess){
buf_166 += res_168.get
pos_167 = mark
res_168 = catPart_164_sub_169
res_168.recover{ _ => reset(pos_167) }
}
 Try(PBranch("catPart_164",buf_166.toSeq)) 
}
catPart_165 <- CLOSE()
 }  yield PBranch("ActionIdent",Seq( catPart_161,catPart_162,catPart_163,catPart_164,catPart_165 )) 
res_160.recoverWith{ case p: ParseError[Char] => 
reset(pos0_159)
Failure( p  )
}
}
res_157.recoverWith{ case err_158: ParseError[Char] =>
reset(pos_156)
Identifier().recoverWith{ case err_174: ParseError[Char] =>
reset(pos_156)
Failure(err_158 ~ err_174 ~  ParseFailed("",pos_156) )
}
}
}


def Literal(): Try[PTree] = {
val pos_175 = mark
val res_176 = {
val pos0_178 = mark
val res_179 = for{
catPart_180 <- expect('\'').map{ char_184 => PLeaf(char_184.toString) }
catPart_181 <- {
def catPart_181_sub_188 = {
val pos0_189 = mark
val res_190 = for{
catPart_191 <- {
val pos_193 = mark
val res_194 = expect('\'')
reset(pos_193)
if( res_194.isSuccess ) Failure(ParseFailed("Neglook failed",pos_193))
 else { Try(PEmpty) }
}
catPart_192 <- Char()
 }  yield PBranch("catPart_181",Seq( catPart_191,catPart_192 )) 
res_190.recoverWith{ case p: ParseError[Char] => 
reset(pos0_189)
Failure( p  )
}
}
var buf_185 = ArrayBuffer.empty[PTree]
var pos_186 = mark
var res_187 = catPart_181_sub_188
res_187.recover{ _ => reset(pos_186) }
while(res_187.isSuccess){
buf_185 += res_187.get
pos_186 = mark
res_187 = catPart_181_sub_188
res_187.recover{ _ => reset(pos_186) }
}
 Try(PBranch("catPart_181",buf_185.toSeq)) 
}
catPart_182 <- expect('\'').map{ char_195 => PLeaf(char_195.toString) }
catPart_183 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_180,catPart_181,catPart_182,catPart_183 )) 
res_179.recoverWith{ case p: ParseError[Char] => 
reset(pos0_178)
Failure( p  )
}
}
res_176.recoverWith{ case err_177: ParseError[Char] =>
reset(pos_175)
val res_196 = {
val pos0_198 = mark
val res_199 = for{
catPart_200 <- expect('"').map{ char_204 => PLeaf(char_204.toString) }
catPart_201 <- {
def catPart_201_sub_208 = {
val pos0_209 = mark
val res_210 = for{
catPart_211 <- {
val pos_213 = mark
val res_214 = expect('"')
reset(pos_213)
if( res_214.isSuccess ) Failure(ParseFailed("Neglook failed",pos_213))
 else { Try(PEmpty) }
}
catPart_212 <- Char()
 }  yield PBranch("catPart_201",Seq( catPart_211,catPart_212 )) 
res_210.recoverWith{ case p: ParseError[Char] => 
reset(pos0_209)
Failure( p  )
}
}
var buf_205 = ArrayBuffer.empty[PTree]
var pos_206 = mark
var res_207 = catPart_201_sub_208
res_207.recover{ _ => reset(pos_206) }
while(res_207.isSuccess){
buf_205 += res_207.get
pos_206 = mark
res_207 = catPart_201_sub_208
res_207.recover{ _ => reset(pos_206) }
}
 Try(PBranch("catPart_201",buf_205.toSeq)) 
}
catPart_202 <- expect('"').map{ char_215 => PLeaf(char_215.toString) }
catPart_203 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_200,catPart_201,catPart_202,catPart_203 )) 
res_199.recoverWith{ case p: ParseError[Char] => 
reset(pos0_198)
Failure( p  )
}
}
res_196.recoverWith{ case err_197: ParseError[Char] =>
reset(pos_175)
val res_216 = {
val pos0_218 = mark
val res_219 = for{
catPart_220 <- expect('`').map{ char_224 => PLeaf(char_224.toString) }
catPart_221 <- {
def catPart_221_sub_228 = {
val pos0_229 = mark
val res_230 = for{
catPart_231 <- {
val pos_233 = mark
val res_234 = expect('`')
reset(pos_233)
if( res_234.isSuccess ) Failure(ParseFailed("Neglook failed",pos_233))
 else { Try(PEmpty) }
}
catPart_232 <- Char()
 }  yield PBranch("catPart_221",Seq( catPart_231,catPart_232 )) 
res_230.recoverWith{ case p: ParseError[Char] => 
reset(pos0_229)
Failure( p  )
}
}
var buf_225 = ArrayBuffer.empty[PTree]
var pos_226 = mark
var res_227 = catPart_221_sub_228
res_227.recover{ _ => reset(pos_226) }
while(res_227.isSuccess){
buf_225 += res_227.get
pos_226 = mark
res_227 = catPart_221_sub_228
res_227.recover{ _ => reset(pos_226) }
}
 Try(PBranch("catPart_221",buf_225.toSeq)) 
}
catPart_222 <- expect('`').map{ char_235 => PLeaf(char_235.toString) }
catPart_223 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_220,catPart_221,catPart_222,catPart_223 )) 
res_219.recoverWith{ case p: ParseError[Char] => 
reset(pos0_218)
Failure( p  )
}
}
res_216.recoverWith{ case err_217: ParseError[Char] =>
reset(pos_175)
Failure(err_177 ~ err_197 ~ err_217 ~  ParseFailed("",pos_175) )
}
}
}
}


def Class(): Try[PTree] = {
val pos0_236 = mark
val res_237 = for{
catPart_238 <- expect('[').map{ char_242 => PLeaf(char_242.toString) }
catPart_239 <- {
def catPart_239_sub_246 = {
val pos0_247 = mark
val res_248 = for{
catPart_249 <- {
val pos_251 = mark
val res_252 = {
val pos_253= mark
val res_254 = for{
char_part_255 <- expect(']')
 } yield PBranch("Lit",Seq( PLeaf(char_part_255.toString) )) 
res_254.recoverWith{ case p: ParseError[Char] =>
reset(pos_253)
 Failure( p ~ ParseFailed("expected ']'",pos_253) ) 
}
}
reset(pos_251)
if( res_252.isSuccess ) Failure(ParseFailed("Neglook failed",pos_251))
 else { Try(PEmpty) }
}
catPart_250 <- Range()
 }  yield PBranch("catPart_239",Seq( catPart_249,catPart_250 )) 
res_248.recoverWith{ case p: ParseError[Char] => 
reset(pos0_247)
Failure( p  )
}
}
var buf_243 = ArrayBuffer.empty[PTree]
var pos_244 = mark
var res_245 = catPart_239_sub_246
res_245.recover{ _ => reset(pos_244) }
while(res_245.isSuccess){
buf_243 += res_245.get
pos_244 = mark
res_245 = catPart_239_sub_246
res_245.recover{ _ => reset(pos_244) }
}
 Try(PBranch("catPart_239",buf_243.toSeq)) 
}
catPart_240 <- expect(']').map{ char_256 => PLeaf(char_256.toString) }
catPart_241 <- Spacing()
 }  yield PBranch("Class",Seq( catPart_238,catPart_239,catPart_240,catPart_241 )) 
res_237.recoverWith{ case p: ParseError[Char] => 
reset(pos0_236)
Failure( p  )
}
}


def Range(): Try[PTree] = {
val pos_257 = mark
val res_258 = {
val pos0_260 = mark
val res_261 = for{
catPart_262 <- Char()
catPart_263 <- expect('-').map{ char_265 => PLeaf(char_265.toString) }
catPart_264 <- Char()
 }  yield PBranch("Range",Seq( catPart_262,catPart_263,catPart_264 )) 
res_261.recoverWith{ case p: ParseError[Char] => 
reset(pos0_260)
Failure( p  )
}
}
res_258.recoverWith{ case err_259: ParseError[Char] =>
reset(pos_257)
Char().recoverWith{ case err_266: ParseError[Char] =>
reset(pos_257)
Failure(err_259 ~ err_266 ~  ParseFailed("",pos_257) )
}
}
}


def Char(): Try[PTree] = {
val pos_267 = mark
val res_268 = {
val pos0_270 = mark
val res_271 = for{
catPart_272 <- expect('\\').map{ char_274 => PLeaf(char_274.toString) }
catPart_273 <- expect('n','[','\\','\'','r',']','t','"').map{ char_275 => PLeaf(char_275.toString) }
 }  yield PBranch("Char",Seq( catPart_272,catPart_273 )) 
res_271.recoverWith{ case p: ParseError[Char] => 
reset(pos0_270)
Failure( p  )
}
}
res_268.recoverWith{ case err_269: ParseError[Char] =>
reset(pos_267)
val res_276 = {
val pos0_278 = mark
val res_279 = for{
catPart_280 <- {
val pos_282 = mark
val res_283 = {
val pos_284= mark
val res_285 = for{
char_part_286 <- expect('\\')
 } yield PBranch("Lit",Seq( PLeaf(char_part_286.toString) )) 
res_285.recoverWith{ case p: ParseError[Char] =>
reset(pos_284)
 Failure( p ~ ParseFailed("expected '\\'",pos_284) ) 
}
}
reset(pos_282)
if( res_283.isSuccess ) Failure(ParseFailed("Neglook failed",pos_282))
 else { Try(PEmpty) }
}
catPart_281 <- any.map{ char_287 => PLeaf(char_287.toString)}
 }  yield PBranch("Char",Seq( catPart_280,catPart_281 )) 
res_279.recoverWith{ case p: ParseError[Char] => 
reset(pos0_278)
Failure( p  )
}
}
res_276.recoverWith{ case err_277: ParseError[Char] =>
reset(pos_267)
Failure(err_269 ~ err_277 ~  ParseFailed("",pos_267) )
}
}
}


def LEFTARROW(): Try[PTree] = {
val pos0_288 = mark
val res_289 = for{
_ <- expect('<')
_ <- expect('-')
 catPart_290 <- Try( PBranch("Lit",Seq(PLeaf('<'.toString),PLeaf('-'.toString))) ) 
catPart_291 <- Spacing()
 }  yield PBranch("LEFTARROW",Seq( catPart_290,catPart_291 )) 
res_289.recoverWith{ case p: ParseError[Char] => 
reset(pos0_288)
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


def AND(): Try[PTree] = {
val pos0_297 = mark
val res_298 = for{
catPart_299 <- expect('&').map{ char_301 => PLeaf(char_301.toString) }
catPart_300 <- Spacing()
 }  yield PBranch("AND",Seq( catPart_299,catPart_300 )) 
res_298.recoverWith{ case p: ParseError[Char] => 
reset(pos0_297)
Failure( p  )
}
}


def NOT(): Try[PTree] = {
val pos0_302 = mark
val res_303 = for{
catPart_304 <- expect('!').map{ char_306 => PLeaf(char_306.toString) }
catPart_305 <- Spacing()
 }  yield PBranch("NOT",Seq( catPart_304,catPart_305 )) 
res_303.recoverWith{ case p: ParseError[Char] => 
reset(pos0_302)
Failure( p  )
}
}


def QUESTION(): Try[PTree] = {
val pos0_307 = mark
val res_308 = for{
catPart_309 <- expect('?').map{ char_311 => PLeaf(char_311.toString) }
catPart_310 <- Spacing()
 }  yield PBranch("QUESTION",Seq( catPart_309,catPart_310 )) 
res_308.recoverWith{ case p: ParseError[Char] => 
reset(pos0_307)
Failure( p  )
}
}


def STAR(): Try[PTree] = {
val pos0_312 = mark
val res_313 = for{
catPart_314 <- expect('*').map{ char_316 => PLeaf(char_316.toString) }
catPart_315 <- Spacing()
 }  yield PBranch("STAR",Seq( catPart_314,catPart_315 )) 
res_313.recoverWith{ case p: ParseError[Char] => 
reset(pos0_312)
Failure( p  )
}
}


def PLUS(): Try[PTree] = {
val pos0_317 = mark
val res_318 = for{
catPart_319 <- expect('+').map{ char_321 => PLeaf(char_321.toString) }
catPart_320 <- Spacing()
 }  yield PBranch("PLUS",Seq( catPart_319,catPart_320 )) 
res_318.recoverWith{ case p: ParseError[Char] => 
reset(pos0_317)
Failure( p  )
}
}


def OPEN(): Try[PTree] = {
val pos0_322 = mark
val res_323 = for{
catPart_324 <- expect('(').map{ char_326 => PLeaf(char_326.toString) }
catPart_325 <- Spacing()
 }  yield PBranch("OPEN",Seq( catPart_324,catPart_325 )) 
res_323.recoverWith{ case p: ParseError[Char] => 
reset(pos0_322)
Failure( p  )
}
}


def CLOSE(): Try[PTree] = {
val pos0_327 = mark
val res_328 = for{
catPart_329 <- expect(')').map{ char_331 => PLeaf(char_331.toString) }
catPart_330 <- Spacing()
 }  yield PBranch("CLOSE",Seq( catPart_329,catPart_330 )) 
res_328.recoverWith{ case p: ParseError[Char] => 
reset(pos0_327)
Failure( p  )
}
}


def DOT(): Try[PTree] = {
val pos0_332 = mark
val res_333 = for{
catPart_334 <- expect('.').map{ char_336 => PLeaf(char_336.toString) }
catPart_335 <- Spacing()
 }  yield PBranch("DOT",Seq( catPart_334,catPart_335 )) 
res_333.recoverWith{ case p: ParseError[Char] => 
reset(pos0_332)
Failure( p  )
}
}


def OPENCURLY(): Try[PTree] = {
val pos0_337 = mark
val res_338 = for{
catPart_339 <- expect('{').map{ char_341 => PLeaf(char_341.toString) }
catPart_340 <- Spacing()
 }  yield PBranch("OPENCURLY",Seq( catPart_339,catPart_340 )) 
res_338.recoverWith{ case p: ParseError[Char] => 
reset(pos0_337)
Failure( p  )
}
}


def CLOSECURLY(): Try[PTree] = {
val pos0_342 = mark
val res_343 = for{
catPart_344 <- expect('}').map{ char_346 => PLeaf(char_346.toString) }
catPart_345 <- Spacing()
 }  yield PBranch("CLOSECURLY",Seq( catPart_344,catPart_345 )) 
res_343.recoverWith{ case p: ParseError[Char] => 
reset(pos0_342)
Failure( p  )
}
}


def VERTBAR(): Try[PTree] = {
val pos0_347 = mark
val res_348 = for{
catPart_349 <- expect('|').map{ char_351 => PLeaf(char_351.toString) }
catPart_350 <- Spacing()
 }  yield PBranch("VERTBAR",Seq( catPart_349,catPart_350 )) 
res_348.recoverWith{ case p: ParseError[Char] => 
reset(pos0_347)
Failure( p  )
}
}


def COMMA(): Try[PTree] = {
val pos0_352 = mark
val res_353 = for{
catPart_354 <- expect(',').map{ char_356 => PLeaf(char_356.toString) }
catPart_355 <- Spacing()
 }  yield PBranch("COMMA",Seq( catPart_354,catPart_355 )) 
res_353.recoverWith{ case p: ParseError[Char] => 
reset(pos0_352)
Failure( p  )
}
}


def Spacing(): Try[PTree] = {
def Spacing_sub_360 = {
val pos_361 = mark
Space().recoverWith{ case err_362: ParseError[Char] =>
reset(pos_361)
Comment().recoverWith{ case err_363: ParseError[Char] =>
reset(pos_361)
Failure(err_362 ~ err_363 ~  ParseFailed("",pos_361) )
}
}
}
var buf_357 = ArrayBuffer.empty[PTree]
var pos_358 = mark
var res_359 = Spacing_sub_360
res_359.recover{ _ => reset(pos_358) }
while(res_359.isSuccess){
buf_357 += res_359.get
pos_358 = mark
res_359 = Spacing_sub_360
res_359.recover{ _ => reset(pos_358) }
}
 Try(PBranch("Spacing",buf_357.toSeq)) 
}


def Comment(): Try[PTree] = {
val pos0_364 = mark
val res_365 = for{
catPart_366 <- expect('#').map{ char_369 => PLeaf(char_369.toString) }
catPart_367 <- {
def catPart_367_sub_373 = {
val pos0_374 = mark
val res_375 = for{
catPart_376 <- {
val pos_378 = mark
val res_379 = EndOfLine()
reset(pos_378)
if( res_379.isSuccess ) Failure(ParseFailed("Neglook failed",pos_378))
 else { Try(PEmpty) }
}
catPart_377 <- any.map{ char_380 => PLeaf(char_380.toString)}
 }  yield PBranch("catPart_367",Seq( catPart_376,catPart_377 )) 
res_375.recoverWith{ case p: ParseError[Char] => 
reset(pos0_374)
Failure( p  )
}
}
var buf_370 = ArrayBuffer.empty[PTree]
var pos_371 = mark
var res_372 = catPart_367_sub_373
res_372.recover{ _ => reset(pos_371) }
while(res_372.isSuccess){
buf_370 += res_372.get
pos_371 = mark
res_372 = catPart_367_sub_373
res_372.recover{ _ => reset(pos_371) }
}
 Try(PBranch("catPart_367",buf_370.toSeq)) 
}
catPart_368 <- EndOfLine()
 }  yield PBranch("Comment",Seq( catPart_366,catPart_367,catPart_368 )) 
res_365.recoverWith{ case p: ParseError[Char] => 
reset(pos0_364)
Failure( p  )
}
}


def Space(): Try[PTree] = {
val pos_381 = mark
expect(' ').map{ char_382 => PLeaf(char_382.toString) }
.recoverWith{ case err_383: ParseError[Char] => 
reset(pos_381)
expect('\t').map{ char_384 => PLeaf(char_384.toString) }
.recoverWith{ case err_385: ParseError[Char] => 
reset(pos_381)
EndOfLine().recoverWith{ case err_386: ParseError[Char] =>
reset(pos_381)
Failure(err_383 ~ err_385 ~ err_386 ~  ParseFailed("",pos_381) )
}
}
}
}


def EndOfLine(): Try[PTree] = {
val pos_387 = mark
val res_388 = {
val pos_390= mark
val res_391 = for{
char_part_392 <- expect('\r')
char_part_393 <- expect('\n')
 } yield PBranch("Lit",Seq( PLeaf(char_part_392.toString) , PLeaf(char_part_393.toString) )) 
res_391.recoverWith{ case p: ParseError[Char] =>
reset(pos_390)
 Failure( p ~ ParseFailed("expected '\r','\n'",pos_390) ) 
}
}
res_388.recoverWith{ case err_389: ParseError[Char] =>
reset(pos_387)
expect('\n').map{ char_394 => PLeaf(char_394.toString) }
.recoverWith{ case err_395: ParseError[Char] => 
reset(pos_387)
expect('\r').map{ char_396 => PLeaf(char_396.toString) }
.recoverWith{ case err_397: ParseError[Char] => 
reset(pos_387)
Failure(err_389 ~ err_395 ~ err_397 ~  ParseFailed("",pos_387) )
}
}
}
}


def EndOfFile(): Try[PTree] = {
val pos_398 = mark
val res_399 = {
val pos_400 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError[Char] =>
reset(pos_400)
 Failure( p ~ ParseFailed("Expected any char",pos_400) )  
}
}
reset(pos_398)
if( res_399.isSuccess ) Failure(ParseFailed("Neglook failed",pos_398))
 else { Try(PEmpty) }
}
}
