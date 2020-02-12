package PEG.PEGParser
import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
import PEG.lexparse.ParseError.implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer){


def Grammar(): Try[PTree] = {
val pos0_2 = mark
val res_3 = for{
catPart_4 <- Spacing()
catPart_5 <- {
val pos0_7 = mark
val res_8 = for{
catPart_9 <- Definition()
catPart_10 <- {
def subMatch_14 = Definition()
var buf_11 = ArrayBuffer.empty[PTree]
var pos_12 = mark
var res_13 = subMatch_14
res_13.recover{ _ => reset(pos_12) }
while(res_13.isSuccess){
buf_11 += res_13.get
pos_12 = mark
res_13 = subMatch_14
res_13.recover{ _ => reset(pos_12) }
}
 Try(PBranch("catPart_10",buf_11.toSeq)) 
}
 }  yield PBranch("catPart_5",Seq( catPart_9,catPart_10 )) 
res_8.recoverWith{ case p: ParseError[Char] => 
reset(pos0_7)
Failure( p  )
}
}
catPart_6 <- EndOfFile()
 }  yield PBranch("Grammar",Seq( catPart_4,catPart_5,catPart_6 )) 
res_3.recoverWith{ case p: ParseError[Char] => 
reset(pos0_2)
Failure( p  )
}
}


def Definition(): Try[PTree] = {
val pos0_16 = mark
val res_17 = for{
catPart_18 <- Identifier()
catPart_19 <- {
val pos_22 = mark
STAR().recoverWith{ case err_23: ParseError[Char] =>
reset(pos_22)
 Try(PEmpty) 
}
}
catPart_20 <- LEFTARROW()
catPart_21 <- ExprWithAction()
 }  yield PBranch("Definition",Seq( catPart_18,catPart_19,catPart_20,catPart_21 )) 
res_17.recoverWith{ case p: ParseError[Char] => 
reset(pos0_16)
Failure( p  )
}
}


def ExprWithAction(): Try[PTree] = {
val pos0_25 = mark
val res_26 = for{
catPart_27 <- ExprPart()
catPart_28 <- {
def subMatch_32 = {
val pos0_33 = mark
val res_34 = for{
catPart_35 <- SLASH()
catPart_36 <- ExprPart()
 }  yield PBranch("catPart_28",Seq( catPart_35,catPart_36 )) 
res_34.recoverWith{ case p: ParseError[Char] => 
reset(pos0_33)
Failure( p  )
}
}
var buf_29 = ArrayBuffer.empty[PTree]
var pos_30 = mark
var res_31 = subMatch_32
res_31.recover{ _ => reset(pos_30) }
while(res_31.isSuccess){
buf_29 += res_31.get
pos_30 = mark
res_31 = subMatch_32
res_31.recover{ _ => reset(pos_30) }
}
 Try(PBranch("catPart_28",buf_29.toSeq)) 
}
 }  yield PBranch("ExprWithAction",Seq( catPart_27,catPart_28 )) 
res_26.recoverWith{ case p: ParseError[Char] => 
reset(pos0_25)
Failure( p  )
}
}


def ExprPart(): Try[PTree] = {
val pos0_38 = mark
val res_39 = for{
catPart_40 <- Sequence()
catPart_41 <- {
val pos_42 = mark
Action().recoverWith{ case err_43: ParseError[Char] =>
reset(pos_42)
 Try(PEmpty) 
}
}
 }  yield PBranch("ExprPart",Seq( catPart_40,catPart_41 )) 
res_39.recoverWith{ case p: ParseError[Char] => 
reset(pos0_38)
Failure( p  )
}
}


def Action(): Try[PTree] = {
val pos0_45 = mark
val res_46 = for{
catPart_47 <- OPENCURLY()
catPart_48 <- {
val pos0_54 = mark
val res_55 = for{
catPart_56 <- {
val pos0_58 = mark
val res_59 = for{
catPart_60 <- {
val pos_62 = mark
val res_63 = expect('|','}')
reset(pos_62)
if( res_63.isSuccess ) Failure(ParseFailed("Neglook failed",pos_62))
 else { Try(PEmpty) }
}
catPart_61 <- any.map{ char_64 => PLeaf(char_64.toString)}
 }  yield PBranch("catPart_56",Seq( catPart_60,catPart_61 )) 
res_59.recoverWith{ case p: ParseError[Char] => 
reset(pos0_58)
Failure( p  )
}
}
catPart_57 <- {
def subMatch_68 = {
val pos0_69 = mark
val res_70 = for{
catPart_71 <- {
val pos_73 = mark
val res_74 = expect('|','}')
reset(pos_73)
if( res_74.isSuccess ) Failure(ParseFailed("Neglook failed",pos_73))
 else { Try(PEmpty) }
}
catPart_72 <- any.map{ char_75 => PLeaf(char_75.toString)}
 }  yield PBranch("catPart_57",Seq( catPart_71,catPart_72 )) 
res_70.recoverWith{ case p: ParseError[Char] => 
reset(pos0_69)
Failure( p  )
}
}
var buf_65 = ArrayBuffer.empty[PTree]
var pos_66 = mark
var res_67 = subMatch_68
res_67.recover{ _ => reset(pos_66) }
while(res_67.isSuccess){
buf_65 += res_67.get
pos_66 = mark
res_67 = subMatch_68
res_67.recover{ _ => reset(pos_66) }
}
 Try(PBranch("catPart_57",buf_65.toSeq)) 
}
 }  yield PBranch("catPart_48",Seq( catPart_56,catPart_57 )) 
res_55.recoverWith{ case p: ParseError[Char] => 
reset(pos0_54)
Failure( p  )
}
}
catPart_49 <- VERTBAR()
catPart_50 <- {
def subMatch_79 = Identifier()
var buf_76 = ArrayBuffer.empty[PTree]
var pos_77 = mark
var res_78 = subMatch_79
res_78.recover{ _ => reset(pos_77) }
while(res_78.isSuccess){
buf_76 += res_78.get
pos_77 = mark
res_78 = subMatch_79
res_78.recover{ _ => reset(pos_77) }
}
 Try(PBranch("catPart_50",buf_76.toSeq)) 
}
catPart_51 <- VERTBAR()
catPart_52 <- {
def subMatch_83 = {
val pos0_84 = mark
val res_85 = for{
catPart_86 <- {
val pos_88 = mark
val res_89 = expect('}')
reset(pos_88)
if( res_89.isSuccess ) Failure(ParseFailed("Neglook failed",pos_88))
 else { Try(PEmpty) }
}
catPart_87 <- any.map{ char_90 => PLeaf(char_90.toString)}
 }  yield PBranch("catPart_52",Seq( catPart_86,catPart_87 )) 
res_85.recoverWith{ case p: ParseError[Char] => 
reset(pos0_84)
Failure( p  )
}
}
var buf_80 = ArrayBuffer.empty[PTree]
var pos_81 = mark
var res_82 = subMatch_83
res_82.recover{ _ => reset(pos_81) }
while(res_82.isSuccess){
buf_80 += res_82.get
pos_81 = mark
res_82 = subMatch_83
res_82.recover{ _ => reset(pos_81) }
}
 Try(PBranch("catPart_52",buf_80.toSeq)) 
}
catPart_53 <- CLOSECURLY()
 }  yield PBranch("Action",Seq( catPart_47,catPart_48,catPart_49,catPart_50,catPart_51,catPart_52,catPart_53 )) 
res_46.recoverWith{ case p: ParseError[Char] => 
reset(pos0_45)
Failure( p  )
}
}


def Expression(): Try[PTree] = {
val pos0_92 = mark
val res_93 = for{
catPart_94 <- Sequence()
catPart_95 <- {
def subMatch_99 = {
val pos0_100 = mark
val res_101 = for{
catPart_102 <- SLASH()
catPart_103 <- Sequence()
 }  yield PBranch("catPart_95",Seq( catPart_102,catPart_103 )) 
res_101.recoverWith{ case p: ParseError[Char] => 
reset(pos0_100)
Failure( p  )
}
}
var buf_96 = ArrayBuffer.empty[PTree]
var pos_97 = mark
var res_98 = subMatch_99
res_98.recover{ _ => reset(pos_97) }
while(res_98.isSuccess){
buf_96 += res_98.get
pos_97 = mark
res_98 = subMatch_99
res_98.recover{ _ => reset(pos_97) }
}
 Try(PBranch("catPart_95",buf_96.toSeq)) 
}
 }  yield PBranch("Expression",Seq( catPart_94,catPart_95 )) 
res_93.recoverWith{ case p: ParseError[Char] => 
reset(pos0_92)
Failure( p  )
}
}


def Sequence(): Try[PTree] = {
def subMatch_108 = Prefix()
var buf_105 = ArrayBuffer.empty[PTree]
var pos_106 = mark
var res_107 = subMatch_108
res_107.recover{ _ => reset(pos_106) }
while(res_107.isSuccess){
buf_105 += res_107.get
pos_106 = mark
res_107 = subMatch_108
res_107.recover{ _ => reset(pos_106) }
}
 Try(PBranch("Sequence",buf_105.toSeq)) 
}


def Prefix(): Try[PTree] = {
val pos0_110 = mark
val res_111 = for{
catPart_112 <- {
val pos_114 = mark
val res_115 = {
val pos_117 = mark
AND().recoverWith{ case err_118: ParseError[Char] =>
reset(pos_117)
NOT().recoverWith{ case err_119: ParseError[Char] =>
reset(pos_117)
Failure(err_118 ~ err_119 ~  ParseFailed("",pos_117) )
}
}
}
res_115.recoverWith{ case err_116: ParseError[Char] =>
reset(pos_114)
 Try(PEmpty) 
}
}
catPart_113 <- Suffix()
 }  yield PBranch("Prefix",Seq( catPart_112,catPart_113 )) 
res_111.recoverWith{ case p: ParseError[Char] => 
reset(pos0_110)
Failure( p  )
}
}


def Suffix(): Try[PTree] = {
val pos0_121 = mark
val res_122 = for{
catPart_123 <- Primary()
catPart_124 <- {
val pos_125 = mark
val res_126 = {
val pos_128 = mark
QUESTION().recoverWith{ case err_129: ParseError[Char] =>
reset(pos_128)
STAR().recoverWith{ case err_130: ParseError[Char] =>
reset(pos_128)
PLUS().recoverWith{ case err_131: ParseError[Char] =>
reset(pos_128)
Failure(err_129 ~ err_130 ~ err_131 ~  ParseFailed("",pos_128) )
}
}
}
}
res_126.recoverWith{ case err_127: ParseError[Char] =>
reset(pos_125)
 Try(PEmpty) 
}
}
 }  yield PBranch("Suffix",Seq( catPart_123,catPart_124 )) 
res_122.recoverWith{ case p: ParseError[Char] => 
reset(pos0_121)
Failure( p  )
}
}


val cache_133 = mutable.HashMap.empty[Int,(Try[PTree],Int)]
def Primary(): Try[PTree] = {
def parser_132(): Try[PTree] = {
val pos_137 = mark
val res_138 = {
val pos0_140 = mark
val res_141 = for{
catPart_142 <- Identifier()
catPart_143 <- {
val pos_144 = mark
val res_145 = {
val pos0_146 = mark
val res_147 = for{
catPart_148 <- {
val pos_150 = mark
STAR().recoverWith{ case err_151: ParseError[Char] =>
reset(pos_150)
 Try(PEmpty) 
}
}
catPart_149 <- LEFTARROW()
 }  yield PBranch("catPart_143",Seq( catPart_148,catPart_149 )) 
res_147.recoverWith{ case p: ParseError[Char] => 
reset(pos0_146)
Failure( p  )
}
}
reset(pos_144)
if( res_145.isSuccess ) Failure(ParseFailed("Neglook failed",pos_144))
 else { Try(PEmpty) }
}
 }  yield PBranch("Primary",Seq( catPart_142,catPart_143 )) 
res_141.recoverWith{ case p: ParseError[Char] => 
reset(pos0_140)
Failure( p  )
}
}
res_138.recoverWith{ case err_139: ParseError[Char] =>
reset(pos_137)
val res_152 = {
val pos0_154 = mark
val res_155 = for{
catPart_156 <- OPEN()
catPart_157 <- Expression()
catPart_158 <- CLOSE()
 }  yield PBranch("Primary",Seq( catPart_156,catPart_157,catPart_158 )) 
res_155.recoverWith{ case p: ParseError[Char] => 
reset(pos0_154)
Failure( p  )
}
}
res_152.recoverWith{ case err_153: ParseError[Char] =>
reset(pos_137)
Literal().recoverWith{ case err_159: ParseError[Char] =>
reset(pos_137)
Class().recoverWith{ case err_160: ParseError[Char] =>
reset(pos_137)
DOT().recoverWith{ case err_161: ParseError[Char] =>
reset(pos_137)
Failure(err_139 ~ err_153 ~ err_159 ~ err_160 ~ err_161 ~  ParseFailed("",pos_137) )
}
}
}
}
}
}
if(!cache_133.contains(mark)){
val init_135 = mark
cache_133(init_135) = parser_132() -> mark
reset(init_135)
}
val (res_134,pos_136) = cache_133(mark)
reset(pos_136)
res_134
}


def Identifier(): Try[PTree] = {
val pos0_163 = mark
val res_164 = for{
catPart_165 <- IdentStart()
catPart_166 <- {
def subMatch_171 = IdentCont()
var buf_168 = ArrayBuffer.empty[PTree]
var pos_169 = mark
var res_170 = subMatch_171
res_170.recover{ _ => reset(pos_169) }
while(res_170.isSuccess){
buf_168 += res_170.get
pos_169 = mark
res_170 = subMatch_171
res_170.recover{ _ => reset(pos_169) }
}
 Try(PBranch("catPart_166",buf_168.toSeq)) 
}
catPart_167 <- Spacing()
 }  yield PBranch("Identifier",Seq( catPart_165,catPart_166,catPart_167 )) 
res_164.recoverWith{ case p: ParseError[Char] => 
reset(pos0_163)
Failure( p  )
}
}


def IdentStart(): Try[PTree] = {
val pos_173 = mark
expect('v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o')
.map{ char_174 => PLeaf(char_174.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_173)
 Failure( p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'",pos_173) ) 
}
}


def IdentCont(): Try[PTree] = {
val pos_176 = mark
IdentStart().recoverWith{ case err_177: ParseError[Char] =>
reset(pos_176)
expect('6','9','2','8','4','0','5','1','7','3')
.map{ char_178 => PLeaf(char_178.toString)}
.recoverWith{ case err_179: ParseError[Char] =>
reset(pos_176)
Failure(err_177 ~ err_179 ~  ParseFailed("",pos_176) )
}
}
}


def Literal(): Try[PTree] = {
val pos_181 = mark
val res_182 = {
val pos0_184 = mark
val res_185 = for{
catPart_186 <- expect('\'').map{ char_190 => PLeaf(char_190.toString) }
catPart_187 <- {
def subMatch_194 = {
val pos0_195 = mark
val res_196 = for{
catPart_197 <- {
val pos_199 = mark
val res_200 = expect('\'')
reset(pos_199)
if( res_200.isSuccess ) Failure(ParseFailed("Neglook failed",pos_199))
 else { Try(PEmpty) }
}
catPart_198 <- Char()
 }  yield PBranch("catPart_187",Seq( catPart_197,catPart_198 )) 
res_196.recoverWith{ case p: ParseError[Char] => 
reset(pos0_195)
Failure( p  )
}
}
var buf_191 = ArrayBuffer.empty[PTree]
var pos_192 = mark
var res_193 = subMatch_194
res_193.recover{ _ => reset(pos_192) }
while(res_193.isSuccess){
buf_191 += res_193.get
pos_192 = mark
res_193 = subMatch_194
res_193.recover{ _ => reset(pos_192) }
}
 Try(PBranch("catPart_187",buf_191.toSeq)) 
}
catPart_188 <- expect('\'').map{ char_201 => PLeaf(char_201.toString) }
catPart_189 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_186,catPart_187,catPart_188,catPart_189 )) 
res_185.recoverWith{ case p: ParseError[Char] => 
reset(pos0_184)
Failure( p  )
}
}
res_182.recoverWith{ case err_183: ParseError[Char] =>
reset(pos_181)
val res_202 = {
val pos0_204 = mark
val res_205 = for{
catPart_206 <- expect('"').map{ char_210 => PLeaf(char_210.toString) }
catPart_207 <- {
def subMatch_214 = {
val pos0_215 = mark
val res_216 = for{
catPart_217 <- {
val pos_219 = mark
val res_220 = expect('"')
reset(pos_219)
if( res_220.isSuccess ) Failure(ParseFailed("Neglook failed",pos_219))
 else { Try(PEmpty) }
}
catPart_218 <- Char()
 }  yield PBranch("catPart_207",Seq( catPart_217,catPart_218 )) 
res_216.recoverWith{ case p: ParseError[Char] => 
reset(pos0_215)
Failure( p  )
}
}
var buf_211 = ArrayBuffer.empty[PTree]
var pos_212 = mark
var res_213 = subMatch_214
res_213.recover{ _ => reset(pos_212) }
while(res_213.isSuccess){
buf_211 += res_213.get
pos_212 = mark
res_213 = subMatch_214
res_213.recover{ _ => reset(pos_212) }
}
 Try(PBranch("catPart_207",buf_211.toSeq)) 
}
catPart_208 <- expect('"').map{ char_221 => PLeaf(char_221.toString) }
catPart_209 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_206,catPart_207,catPart_208,catPart_209 )) 
res_205.recoverWith{ case p: ParseError[Char] => 
reset(pos0_204)
Failure( p  )
}
}
res_202.recoverWith{ case err_203: ParseError[Char] =>
reset(pos_181)
val res_222 = {
val pos0_224 = mark
val res_225 = for{
catPart_226 <- expect('`').map{ char_230 => PLeaf(char_230.toString) }
catPart_227 <- {
def subMatch_234 = {
val pos0_235 = mark
val res_236 = for{
catPart_237 <- {
val pos_239 = mark
val res_240 = expect('`')
reset(pos_239)
if( res_240.isSuccess ) Failure(ParseFailed("Neglook failed",pos_239))
 else { Try(PEmpty) }
}
catPart_238 <- Char()
 }  yield PBranch("catPart_227",Seq( catPart_237,catPart_238 )) 
res_236.recoverWith{ case p: ParseError[Char] => 
reset(pos0_235)
Failure( p  )
}
}
var buf_231 = ArrayBuffer.empty[PTree]
var pos_232 = mark
var res_233 = subMatch_234
res_233.recover{ _ => reset(pos_232) }
while(res_233.isSuccess){
buf_231 += res_233.get
pos_232 = mark
res_233 = subMatch_234
res_233.recover{ _ => reset(pos_232) }
}
 Try(PBranch("catPart_227",buf_231.toSeq)) 
}
catPart_228 <- expect('`').map{ char_241 => PLeaf(char_241.toString) }
catPart_229 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_226,catPart_227,catPart_228,catPart_229 )) 
res_225.recoverWith{ case p: ParseError[Char] => 
reset(pos0_224)
Failure( p  )
}
}
res_222.recoverWith{ case err_223: ParseError[Char] =>
reset(pos_181)
Failure(err_183 ~ err_203 ~ err_223 ~  ParseFailed("",pos_181) )
}
}
}
}


def Class(): Try[PTree] = {
val pos0_243 = mark
val res_244 = for{
catPart_245 <- expect('[').map{ char_249 => PLeaf(char_249.toString) }
catPart_246 <- {
def subMatch_253 = {
val pos0_254 = mark
val res_255 = for{
catPart_256 <- {
val pos_258 = mark
val res_259 = {
val pos_260= mark
val res_261 = for{
char_part_262 <- expect(']')
 } yield PBranch("Lit",Seq( PLeaf(char_part_262.toString) )) 
res_261.recoverWith{ case p: ParseError[Char] =>
reset(pos_260)
 Failure( p ~ ParseFailed("expected ']'",pos_260) ) 
}
}
reset(pos_258)
if( res_259.isSuccess ) Failure(ParseFailed("Neglook failed",pos_258))
 else { Try(PEmpty) }
}
catPart_257 <- Range()
 }  yield PBranch("catPart_246",Seq( catPart_256,catPart_257 )) 
res_255.recoverWith{ case p: ParseError[Char] => 
reset(pos0_254)
Failure( p  )
}
}
var buf_250 = ArrayBuffer.empty[PTree]
var pos_251 = mark
var res_252 = subMatch_253
res_252.recover{ _ => reset(pos_251) }
while(res_252.isSuccess){
buf_250 += res_252.get
pos_251 = mark
res_252 = subMatch_253
res_252.recover{ _ => reset(pos_251) }
}
 Try(PBranch("catPart_246",buf_250.toSeq)) 
}
catPart_247 <- expect(']').map{ char_263 => PLeaf(char_263.toString) }
catPart_248 <- Spacing()
 }  yield PBranch("Class",Seq( catPart_245,catPart_246,catPart_247,catPart_248 )) 
res_244.recoverWith{ case p: ParseError[Char] => 
reset(pos0_243)
Failure( p  )
}
}


def Range(): Try[PTree] = {
val pos_265 = mark
val res_266 = {
val pos0_268 = mark
val res_269 = for{
catPart_270 <- Char()
catPart_271 <- expect('-').map{ char_273 => PLeaf(char_273.toString) }
catPart_272 <- Char()
 }  yield PBranch("Range",Seq( catPart_270,catPart_271,catPart_272 )) 
res_269.recoverWith{ case p: ParseError[Char] => 
reset(pos0_268)
Failure( p  )
}
}
res_266.recoverWith{ case err_267: ParseError[Char] =>
reset(pos_265)
Char().recoverWith{ case err_274: ParseError[Char] =>
reset(pos_265)
Failure(err_267 ~ err_274 ~  ParseFailed("",pos_265) )
}
}
}


def Char(): Try[PTree] = {
val pos_276 = mark
val res_277 = {
val pos0_279 = mark
val res_280 = for{
catPart_281 <- expect('\\').map{ char_283 => PLeaf(char_283.toString) }
catPart_282 <- expect('n','[','\\','\'','r',']','t','"').map{ char_284 => PLeaf(char_284.toString) }
 }  yield PBranch("Char",Seq( catPart_281,catPart_282 )) 
res_280.recoverWith{ case p: ParseError[Char] => 
reset(pos0_279)
Failure( p  )
}
}
res_277.recoverWith{ case err_278: ParseError[Char] =>
reset(pos_276)
val res_285 = {
val pos0_287 = mark
val res_288 = for{
catPart_289 <- {
val pos_291 = mark
val res_292 = {
val pos_293= mark
val res_294 = for{
char_part_295 <- expect('\\')
 } yield PBranch("Lit",Seq( PLeaf(char_part_295.toString) )) 
res_294.recoverWith{ case p: ParseError[Char] =>
reset(pos_293)
 Failure( p ~ ParseFailed("expected '\\'",pos_293) ) 
}
}
reset(pos_291)
if( res_292.isSuccess ) Failure(ParseFailed("Neglook failed",pos_291))
 else { Try(PEmpty) }
}
catPart_290 <- any.map{ char_296 => PLeaf(char_296.toString)}
 }  yield PBranch("Char",Seq( catPart_289,catPart_290 )) 
res_288.recoverWith{ case p: ParseError[Char] => 
reset(pos0_287)
Failure( p  )
}
}
res_285.recoverWith{ case err_286: ParseError[Char] =>
reset(pos_276)
Failure(err_278 ~ err_286 ~  ParseFailed("",pos_276) )
}
}
}


def LEFTARROW(): Try[PTree] = {
val pos0_298 = mark
val res_299 = for{
_ <- expect('<')
_ <- expect('-')
 catPart_300 <- Try( PBranch("Lit",Seq(PLeaf('<'.toString),PLeaf('-'.toString))) ) 
catPart_301 <- Spacing()
 }  yield PBranch("LEFTARROW",Seq( catPart_300,catPart_301 )) 
res_299.recoverWith{ case p: ParseError[Char] => 
reset(pos0_298)
Failure( p  )
}
}


def SLASH(): Try[PTree] = {
val pos0_303 = mark
val res_304 = for{
catPart_305 <- expect('/').map{ char_307 => PLeaf(char_307.toString) }
catPart_306 <- Spacing()
 }  yield PBranch("SLASH",Seq( catPart_305,catPart_306 )) 
res_304.recoverWith{ case p: ParseError[Char] => 
reset(pos0_303)
Failure( p  )
}
}


def AND(): Try[PTree] = {
val pos0_309 = mark
val res_310 = for{
catPart_311 <- expect('&').map{ char_313 => PLeaf(char_313.toString) }
catPart_312 <- Spacing()
 }  yield PBranch("AND",Seq( catPart_311,catPart_312 )) 
res_310.recoverWith{ case p: ParseError[Char] => 
reset(pos0_309)
Failure( p  )
}
}


def NOT(): Try[PTree] = {
val pos0_315 = mark
val res_316 = for{
catPart_317 <- expect('!').map{ char_319 => PLeaf(char_319.toString) }
catPart_318 <- Spacing()
 }  yield PBranch("NOT",Seq( catPart_317,catPart_318 )) 
res_316.recoverWith{ case p: ParseError[Char] => 
reset(pos0_315)
Failure( p  )
}
}


def QUESTION(): Try[PTree] = {
val pos0_321 = mark
val res_322 = for{
catPart_323 <- expect('?').map{ char_325 => PLeaf(char_325.toString) }
catPart_324 <- Spacing()
 }  yield PBranch("QUESTION",Seq( catPart_323,catPart_324 )) 
res_322.recoverWith{ case p: ParseError[Char] => 
reset(pos0_321)
Failure( p  )
}
}


def STAR(): Try[PTree] = {
val pos0_327 = mark
val res_328 = for{
catPart_329 <- expect('*').map{ char_331 => PLeaf(char_331.toString) }
catPart_330 <- Spacing()
 }  yield PBranch("STAR",Seq( catPart_329,catPart_330 )) 
res_328.recoverWith{ case p: ParseError[Char] => 
reset(pos0_327)
Failure( p  )
}
}


def PLUS(): Try[PTree] = {
val pos0_333 = mark
val res_334 = for{
catPart_335 <- expect('+').map{ char_337 => PLeaf(char_337.toString) }
catPart_336 <- Spacing()
 }  yield PBranch("PLUS",Seq( catPart_335,catPart_336 )) 
res_334.recoverWith{ case p: ParseError[Char] => 
reset(pos0_333)
Failure( p  )
}
}


def OPEN(): Try[PTree] = {
val pos0_339 = mark
val res_340 = for{
catPart_341 <- expect('(').map{ char_343 => PLeaf(char_343.toString) }
catPart_342 <- Spacing()
 }  yield PBranch("OPEN",Seq( catPart_341,catPart_342 )) 
res_340.recoverWith{ case p: ParseError[Char] => 
reset(pos0_339)
Failure( p  )
}
}


def CLOSE(): Try[PTree] = {
val pos0_345 = mark
val res_346 = for{
catPart_347 <- expect(')').map{ char_349 => PLeaf(char_349.toString) }
catPart_348 <- Spacing()
 }  yield PBranch("CLOSE",Seq( catPart_347,catPart_348 )) 
res_346.recoverWith{ case p: ParseError[Char] => 
reset(pos0_345)
Failure( p  )
}
}


def DOT(): Try[PTree] = {
val pos0_351 = mark
val res_352 = for{
catPart_353 <- expect('.').map{ char_355 => PLeaf(char_355.toString) }
catPart_354 <- Spacing()
 }  yield PBranch("DOT",Seq( catPart_353,catPart_354 )) 
res_352.recoverWith{ case p: ParseError[Char] => 
reset(pos0_351)
Failure( p  )
}
}


def OPENCURLY(): Try[PTree] = {
val pos0_357 = mark
val res_358 = for{
catPart_359 <- expect('{').map{ char_361 => PLeaf(char_361.toString) }
catPart_360 <- Spacing()
 }  yield PBranch("OPENCURLY",Seq( catPart_359,catPart_360 )) 
res_358.recoverWith{ case p: ParseError[Char] => 
reset(pos0_357)
Failure( p  )
}
}


def CLOSECURLY(): Try[PTree] = {
val pos0_363 = mark
val res_364 = for{
catPart_365 <- expect('}').map{ char_367 => PLeaf(char_367.toString) }
catPart_366 <- Spacing()
 }  yield PBranch("CLOSECURLY",Seq( catPart_365,catPart_366 )) 
res_364.recoverWith{ case p: ParseError[Char] => 
reset(pos0_363)
Failure( p  )
}
}


def VERTBAR(): Try[PTree] = {
val pos0_369 = mark
val res_370 = for{
catPart_371 <- expect('|').map{ char_373 => PLeaf(char_373.toString) }
catPart_372 <- Spacing()
 }  yield PBranch("VERTBAR",Seq( catPart_371,catPart_372 )) 
res_370.recoverWith{ case p: ParseError[Char] => 
reset(pos0_369)
Failure( p  )
}
}


def Spacing(): Try[PTree] = {
def subMatch_378 = {
val pos_379 = mark
Space().recoverWith{ case err_380: ParseError[Char] =>
reset(pos_379)
Comment().recoverWith{ case err_381: ParseError[Char] =>
reset(pos_379)
Failure(err_380 ~ err_381 ~  ParseFailed("",pos_379) )
}
}
}
var buf_375 = ArrayBuffer.empty[PTree]
var pos_376 = mark
var res_377 = subMatch_378
res_377.recover{ _ => reset(pos_376) }
while(res_377.isSuccess){
buf_375 += res_377.get
pos_376 = mark
res_377 = subMatch_378
res_377.recover{ _ => reset(pos_376) }
}
 Try(PBranch("Spacing",buf_375.toSeq)) 
}


def Comment(): Try[PTree] = {
val pos0_383 = mark
val res_384 = for{
catPart_385 <- expect('#').map{ char_388 => PLeaf(char_388.toString) }
catPart_386 <- {
def subMatch_392 = {
val pos0_393 = mark
val res_394 = for{
catPart_395 <- {
val pos_397 = mark
val res_398 = EndOfLine()
reset(pos_397)
if( res_398.isSuccess ) Failure(ParseFailed("Neglook failed",pos_397))
 else { Try(PEmpty) }
}
catPart_396 <- any.map{ char_399 => PLeaf(char_399.toString)}
 }  yield PBranch("catPart_386",Seq( catPart_395,catPart_396 )) 
res_394.recoverWith{ case p: ParseError[Char] => 
reset(pos0_393)
Failure( p  )
}
}
var buf_389 = ArrayBuffer.empty[PTree]
var pos_390 = mark
var res_391 = subMatch_392
res_391.recover{ _ => reset(pos_390) }
while(res_391.isSuccess){
buf_389 += res_391.get
pos_390 = mark
res_391 = subMatch_392
res_391.recover{ _ => reset(pos_390) }
}
 Try(PBranch("catPart_386",buf_389.toSeq)) 
}
catPart_387 <- EndOfLine()
 }  yield PBranch("Comment",Seq( catPart_385,catPart_386,catPart_387 )) 
res_384.recoverWith{ case p: ParseError[Char] => 
reset(pos0_383)
Failure( p  )
}
}


def Space(): Try[PTree] = {
val pos_401 = mark
expect(' ').map{ char_402 => PLeaf(char_402.toString) }
.recoverWith{ case err_403: ParseError[Char] => 
reset(pos_401)
expect('\t').map{ char_404 => PLeaf(char_404.toString) }
.recoverWith{ case err_405: ParseError[Char] => 
reset(pos_401)
EndOfLine().recoverWith{ case err_406: ParseError[Char] =>
reset(pos_401)
Failure(err_403 ~ err_405 ~ err_406 ~  ParseFailed("",pos_401) )
}
}
}
}


def EndOfLine(): Try[PTree] = {
val pos_408 = mark
val res_409 = {
val pos_411= mark
val res_412 = for{
char_part_413 <- expect('\r')
char_part_414 <- expect('\n')
 } yield PBranch("Lit",Seq( PLeaf(char_part_413.toString) , PLeaf(char_part_414.toString) )) 
res_412.recoverWith{ case p: ParseError[Char] =>
reset(pos_411)
 Failure( p ~ ParseFailed("expected '\r','\n'",pos_411) ) 
}
}
res_409.recoverWith{ case err_410: ParseError[Char] =>
reset(pos_408)
expect('\n').map{ char_415 => PLeaf(char_415.toString) }
.recoverWith{ case err_416: ParseError[Char] => 
reset(pos_408)
expect('\r').map{ char_417 => PLeaf(char_417.toString) }
.recoverWith{ case err_418: ParseError[Char] => 
reset(pos_408)
Failure(err_410 ~ err_416 ~ err_418 ~  ParseFailed("",pos_408) )
}
}
}
}


def EndOfFile(): Try[PTree] = {
val pos_420 = mark
val res_421 = {
val pos_422 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError[Char] =>
reset(pos_422)
 Failure( p ~ ParseFailed("Expected any char",pos_422) )  
}
}
reset(pos_420)
if( res_421.isSuccess ) Failure(ParseFailed("Neglook failed",pos_420))
 else { Try(PEmpty) }
}
}
