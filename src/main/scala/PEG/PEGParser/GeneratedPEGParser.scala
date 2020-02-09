package PEG.PEGParser
import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
import PEG.lexparse.ParseError.implicits._
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
def subMatch_13 = Definition()
var buf_10 = ArrayBuffer.empty[PTree]
var pos_11 = mark
var res_12 = subMatch_13
res_12.recover{ _ => reset(pos_11) }
while(res_12.isSuccess){
buf_10 += res_12.get
pos_11 = mark
res_12 = subMatch_13
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
def subMatch_29 = {
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
var res_28 = subMatch_29
res_28.recover{ _ => reset(pos_27) }
while(res_28.isSuccess){
buf_26 += res_28.get
pos_27 = mark
res_28 = subMatch_29
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
def subMatch_37 = Prefix()
var buf_34 = ArrayBuffer.empty[PTree]
var pos_35 = mark
var res_36 = subMatch_37
res_36.recover{ _ => reset(pos_35) }
while(res_36.isSuccess){
buf_34 += res_36.get
pos_35 = mark
res_36 = subMatch_37
res_36.recover{ _ => reset(pos_35) }
}
 Try(PBranch("Sequence",buf_34.toSeq)) 
}


def Prefix(): Try[PTree] = {
val pos0_38 = mark
val res_39 = for{
catPart_40 <- {
val pos_42 = mark
val res_43 = {
val pos_45 = mark
AND().recoverWith{ case err_46: ParseError[Char] =>
reset(pos_45)
NOT().recoverWith{ case err_47: ParseError[Char] =>
reset(pos_45)
Failure(err_46 ~ err_47 ~  ParseFailed("",pos_45) )
}
}
}
res_43.recoverWith{ case err_44: ParseError[Char] =>
reset(pos_42)
 Try(PEmpty) 
}
}
catPart_41 <- Suffix()
 }  yield PBranch("Prefix",Seq( catPart_40,catPart_41 )) 
res_39.recoverWith{ case p: ParseError[Char] => 
reset(pos0_38)
Failure( p  )
}
}


def Suffix(): Try[PTree] = {
val pos0_48 = mark
val res_49 = for{
catPart_50 <- Primary()
catPart_51 <- {
val pos_52 = mark
val res_53 = {
val pos_55 = mark
QUESTION().recoverWith{ case err_56: ParseError[Char] =>
reset(pos_55)
STAR().recoverWith{ case err_57: ParseError[Char] =>
reset(pos_55)
PLUS().recoverWith{ case err_58: ParseError[Char] =>
reset(pos_55)
Failure(err_56 ~ err_57 ~ err_58 ~  ParseFailed("",pos_55) )
}
}
}
}
res_53.recoverWith{ case err_54: ParseError[Char] =>
reset(pos_52)
 Try(PEmpty) 
}
}
 }  yield PBranch("Suffix",Seq( catPart_50,catPart_51 )) 
res_49.recoverWith{ case p: ParseError[Char] => 
reset(pos0_48)
Failure( p  )
}
}


def Primary(): Try[PTree] = {
val pos_59 = mark
val res_60 = {
val pos0_62 = mark
val res_63 = for{
catPart_64 <- Identifier()
catPart_65 <- {
val pos_66 = mark
val res_67 = LEFTARROW()
reset(pos_66)
if( res_67.isSuccess ) Failure(ParseFailed("Neglook failed",pos_66))
 else { Try(PEmpty) }
}
 }  yield PBranch("Primary",Seq( catPart_64,catPart_65 )) 
res_63.recoverWith{ case p: ParseError[Char] => 
reset(pos0_62)
Failure( p  )
}
}
res_60.recoverWith{ case err_61: ParseError[Char] =>
reset(pos_59)
val res_68 = {
val pos0_70 = mark
val res_71 = for{
catPart_72 <- OPEN()
catPart_73 <- Expression()
catPart_74 <- CLOSE()
 }  yield PBranch("Primary",Seq( catPart_72,catPart_73,catPart_74 )) 
res_71.recoverWith{ case p: ParseError[Char] => 
reset(pos0_70)
Failure( p  )
}
}
res_68.recoverWith{ case err_69: ParseError[Char] =>
reset(pos_59)
Literal().recoverWith{ case err_75: ParseError[Char] =>
reset(pos_59)
Class().recoverWith{ case err_76: ParseError[Char] =>
reset(pos_59)
DOT().recoverWith{ case err_77: ParseError[Char] =>
reset(pos_59)
Failure(err_61 ~ err_69 ~ err_75 ~ err_76 ~ err_77 ~  ParseFailed("",pos_59) )
}
}
}
}
}
}


def Identifier(): Try[PTree] = {
val pos0_78 = mark
val res_79 = for{
catPart_80 <- IdentStart()
catPart_81 <- {
def subMatch_86 = IdentCont()
var buf_83 = ArrayBuffer.empty[PTree]
var pos_84 = mark
var res_85 = subMatch_86
res_85.recover{ _ => reset(pos_84) }
while(res_85.isSuccess){
buf_83 += res_85.get
pos_84 = mark
res_85 = subMatch_86
res_85.recover{ _ => reset(pos_84) }
}
 Try(PBranch("catPart_81",buf_83.toSeq)) 
}
catPart_82 <- Spacing()
 }  yield PBranch("Identifier",Seq( catPart_80,catPart_81,catPart_82 )) 
res_79.recoverWith{ case p: ParseError[Char] => 
reset(pos0_78)
Failure( p  )
}
}


def IdentStart(): Try[PTree] = {
val pos_87 = mark
expect('v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o')
.map{ char_88 => PLeaf(char_88.toString)}
.recoverWith{ case p: ParseError[Char] =>
reset(pos_87)
 Failure( p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'",pos_87) ) 
}
}


def IdentCont(): Try[PTree] = {
val pos_89 = mark
IdentStart().recoverWith{ case err_90: ParseError[Char] =>
reset(pos_89)
expect('6','9','2','8','4','0','5','1','7','3')
.map{ char_91 => PLeaf(char_91.toString)}
.recoverWith{ case err_92: ParseError[Char] =>
reset(pos_89)
Failure(err_90 ~ err_92 ~  ParseFailed("",pos_89) )
}
}
}


def Literal(): Try[PTree] = {
val pos_93 = mark
val res_94 = {
val pos0_96 = mark
val res_97 = for{
catPart_98 <- expect('\'').map{ char_102 => PLeaf(char_102.toString) }
catPart_99 <- {
def subMatch_106 = {
val pos0_107 = mark
val res_108 = for{
catPart_109 <- {
val pos_111 = mark
val res_112 = expect('\'')
reset(pos_111)
if( res_112.isSuccess ) Failure(ParseFailed("Neglook failed",pos_111))
 else { Try(PEmpty) }
}
catPart_110 <- Char()
 }  yield PBranch("catPart_99",Seq( catPart_109,catPart_110 )) 
res_108.recoverWith{ case p: ParseError[Char] => 
reset(pos0_107)
Failure( p  )
}
}
var buf_103 = ArrayBuffer.empty[PTree]
var pos_104 = mark
var res_105 = subMatch_106
res_105.recover{ _ => reset(pos_104) }
while(res_105.isSuccess){
buf_103 += res_105.get
pos_104 = mark
res_105 = subMatch_106
res_105.recover{ _ => reset(pos_104) }
}
 Try(PBranch("catPart_99",buf_103.toSeq)) 
}
catPart_100 <- expect('\'').map{ char_113 => PLeaf(char_113.toString) }
catPart_101 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_98,catPart_99,catPart_100,catPart_101 )) 
res_97.recoverWith{ case p: ParseError[Char] => 
reset(pos0_96)
Failure( p  )
}
}
res_94.recoverWith{ case err_95: ParseError[Char] =>
reset(pos_93)
val res_114 = {
val pos0_116 = mark
val res_117 = for{
catPart_118 <- expect('"').map{ char_122 => PLeaf(char_122.toString) }
catPart_119 <- {
def subMatch_126 = {
val pos0_127 = mark
val res_128 = for{
catPart_129 <- {
val pos_131 = mark
val res_132 = expect('"')
reset(pos_131)
if( res_132.isSuccess ) Failure(ParseFailed("Neglook failed",pos_131))
 else { Try(PEmpty) }
}
catPart_130 <- Char()
 }  yield PBranch("catPart_119",Seq( catPart_129,catPart_130 )) 
res_128.recoverWith{ case p: ParseError[Char] => 
reset(pos0_127)
Failure( p  )
}
}
var buf_123 = ArrayBuffer.empty[PTree]
var pos_124 = mark
var res_125 = subMatch_126
res_125.recover{ _ => reset(pos_124) }
while(res_125.isSuccess){
buf_123 += res_125.get
pos_124 = mark
res_125 = subMatch_126
res_125.recover{ _ => reset(pos_124) }
}
 Try(PBranch("catPart_119",buf_123.toSeq)) 
}
catPart_120 <- expect('"').map{ char_133 => PLeaf(char_133.toString) }
catPart_121 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_118,catPart_119,catPart_120,catPart_121 )) 
res_117.recoverWith{ case p: ParseError[Char] => 
reset(pos0_116)
Failure( p  )
}
}
res_114.recoverWith{ case err_115: ParseError[Char] =>
reset(pos_93)
val res_134 = {
val pos0_136 = mark
val res_137 = for{
catPart_138 <- expect('`').map{ char_142 => PLeaf(char_142.toString) }
catPart_139 <- {
def subMatch_146 = {
val pos0_147 = mark
val res_148 = for{
catPart_149 <- {
val pos_151 = mark
val res_152 = expect('`')
reset(pos_151)
if( res_152.isSuccess ) Failure(ParseFailed("Neglook failed",pos_151))
 else { Try(PEmpty) }
}
catPart_150 <- Char()
 }  yield PBranch("catPart_139",Seq( catPart_149,catPart_150 )) 
res_148.recoverWith{ case p: ParseError[Char] => 
reset(pos0_147)
Failure( p  )
}
}
var buf_143 = ArrayBuffer.empty[PTree]
var pos_144 = mark
var res_145 = subMatch_146
res_145.recover{ _ => reset(pos_144) }
while(res_145.isSuccess){
buf_143 += res_145.get
pos_144 = mark
res_145 = subMatch_146
res_145.recover{ _ => reset(pos_144) }
}
 Try(PBranch("catPart_139",buf_143.toSeq)) 
}
catPart_140 <- expect('`').map{ char_153 => PLeaf(char_153.toString) }
catPart_141 <- Spacing()
 }  yield PBranch("Literal",Seq( catPart_138,catPart_139,catPart_140,catPart_141 )) 
res_137.recoverWith{ case p: ParseError[Char] => 
reset(pos0_136)
Failure( p  )
}
}
res_134.recoverWith{ case err_135: ParseError[Char] =>
reset(pos_93)
Failure(err_95 ~ err_115 ~ err_135 ~  ParseFailed("",pos_93) )
}
}
}
}


def Class(): Try[PTree] = {
val pos0_154 = mark
val res_155 = for{
catPart_156 <- expect('[').map{ char_160 => PLeaf(char_160.toString) }
catPart_157 <- {
def subMatch_164 = {
val pos0_165 = mark
val res_166 = for{
catPart_167 <- {
val pos_169 = mark
val res_170 = {
val pos_171= mark
val res_172 = for{
char_part_173 <- expect(']')
 } yield PBranch("Lit",Seq( PLeaf(char_part_173.toString) )) 
res_172.recoverWith{ case p: ParseError[Char] =>
reset(pos_171)
 Failure( p ~ ParseFailed("expected ']'",pos_171) ) 
}
}
reset(pos_169)
if( res_170.isSuccess ) Failure(ParseFailed("Neglook failed",pos_169))
 else { Try(PEmpty) }
}
catPart_168 <- Range()
 }  yield PBranch("catPart_157",Seq( catPart_167,catPart_168 )) 
res_166.recoverWith{ case p: ParseError[Char] => 
reset(pos0_165)
Failure( p  )
}
}
var buf_161 = ArrayBuffer.empty[PTree]
var pos_162 = mark
var res_163 = subMatch_164
res_163.recover{ _ => reset(pos_162) }
while(res_163.isSuccess){
buf_161 += res_163.get
pos_162 = mark
res_163 = subMatch_164
res_163.recover{ _ => reset(pos_162) }
}
 Try(PBranch("catPart_157",buf_161.toSeq)) 
}
catPart_158 <- expect(']').map{ char_174 => PLeaf(char_174.toString) }
catPart_159 <- Spacing()
 }  yield PBranch("Class",Seq( catPart_156,catPart_157,catPart_158,catPart_159 )) 
res_155.recoverWith{ case p: ParseError[Char] => 
reset(pos0_154)
Failure( p  )
}
}


def Range(): Try[PTree] = {
val pos_175 = mark
val res_176 = {
val pos0_178 = mark
val res_179 = for{
catPart_180 <- Char()
catPart_181 <- expect('-').map{ char_183 => PLeaf(char_183.toString) }
catPart_182 <- Char()
 }  yield PBranch("Range",Seq( catPart_180,catPart_181,catPart_182 )) 
res_179.recoverWith{ case p: ParseError[Char] => 
reset(pos0_178)
Failure( p  )
}
}
res_176.recoverWith{ case err_177: ParseError[Char] =>
reset(pos_175)
Char().recoverWith{ case err_184: ParseError[Char] =>
reset(pos_175)
Failure(err_177 ~ err_184 ~  ParseFailed("",pos_175) )
}
}
}


def Char(): Try[PTree] = {
val pos_185 = mark
val res_186 = {
val pos0_188 = mark
val res_189 = for{
catPart_190 <- expect('\\').map{ char_192 => PLeaf(char_192.toString) }
catPart_191 <- expect('n','[','\\','\'','r',']','t','"').map{ char_193 => PLeaf(char_193.toString) }
 }  yield PBranch("Char",Seq( catPart_190,catPart_191 )) 
res_189.recoverWith{ case p: ParseError[Char] => 
reset(pos0_188)
Failure( p  )
}
}
res_186.recoverWith{ case err_187: ParseError[Char] =>
reset(pos_185)
val res_194 = {
val pos0_196 = mark
val res_197 = for{
catPart_198 <- {
val pos_200 = mark
val res_201 = {
val pos_202= mark
val res_203 = for{
char_part_204 <- expect('\\')
 } yield PBranch("Lit",Seq( PLeaf(char_part_204.toString) )) 
res_203.recoverWith{ case p: ParseError[Char] =>
reset(pos_202)
 Failure( p ~ ParseFailed("expected '\\'",pos_202) ) 
}
}
reset(pos_200)
if( res_201.isSuccess ) Failure(ParseFailed("Neglook failed",pos_200))
 else { Try(PEmpty) }
}
catPart_199 <- any.map{ char_205 => PLeaf(char_205.toString)}
 }  yield PBranch("Char",Seq( catPart_198,catPart_199 )) 
res_197.recoverWith{ case p: ParseError[Char] => 
reset(pos0_196)
Failure( p  )
}
}
res_194.recoverWith{ case err_195: ParseError[Char] =>
reset(pos_185)
Failure(err_187 ~ err_195 ~  ParseFailed("",pos_185) )
}
}
}


def LEFTARROW(): Try[PTree] = {
val pos0_206 = mark
val res_207 = for{
_ <- expect('<')
_ <- expect('-')
 catPart_208 <- Try( PBranch("Lit",Seq(PLeaf('<'.toString),PLeaf('-'.toString))) ) 
catPart_209 <- Spacing()
 }  yield PBranch("LEFTARROW",Seq( catPart_208,catPart_209 )) 
res_207.recoverWith{ case p: ParseError[Char] => 
reset(pos0_206)
Failure( p  )
}
}


def SLASH(): Try[PTree] = {
val pos0_210 = mark
val res_211 = for{
catPart_212 <- expect('/').map{ char_214 => PLeaf(char_214.toString) }
catPart_213 <- Spacing()
 }  yield PBranch("SLASH",Seq( catPart_212,catPart_213 )) 
res_211.recoverWith{ case p: ParseError[Char] => 
reset(pos0_210)
Failure( p  )
}
}


def AND(): Try[PTree] = {
val pos0_215 = mark
val res_216 = for{
catPart_217 <- expect('&').map{ char_219 => PLeaf(char_219.toString) }
catPart_218 <- Spacing()
 }  yield PBranch("AND",Seq( catPart_217,catPart_218 )) 
res_216.recoverWith{ case p: ParseError[Char] => 
reset(pos0_215)
Failure( p  )
}
}


def NOT(): Try[PTree] = {
val pos0_220 = mark
val res_221 = for{
catPart_222 <- expect('!').map{ char_224 => PLeaf(char_224.toString) }
catPart_223 <- Spacing()
 }  yield PBranch("NOT",Seq( catPart_222,catPart_223 )) 
res_221.recoverWith{ case p: ParseError[Char] => 
reset(pos0_220)
Failure( p  )
}
}


def QUESTION(): Try[PTree] = {
val pos0_225 = mark
val res_226 = for{
catPart_227 <- expect('?').map{ char_229 => PLeaf(char_229.toString) }
catPart_228 <- Spacing()
 }  yield PBranch("QUESTION",Seq( catPart_227,catPart_228 )) 
res_226.recoverWith{ case p: ParseError[Char] => 
reset(pos0_225)
Failure( p  )
}
}


def STAR(): Try[PTree] = {
val pos0_230 = mark
val res_231 = for{
catPart_232 <- expect('*').map{ char_234 => PLeaf(char_234.toString) }
catPart_233 <- Spacing()
 }  yield PBranch("STAR",Seq( catPart_232,catPart_233 )) 
res_231.recoverWith{ case p: ParseError[Char] => 
reset(pos0_230)
Failure( p  )
}
}


def PLUS(): Try[PTree] = {
val pos0_235 = mark
val res_236 = for{
catPart_237 <- expect('+').map{ char_239 => PLeaf(char_239.toString) }
catPart_238 <- Spacing()
 }  yield PBranch("PLUS",Seq( catPart_237,catPart_238 )) 
res_236.recoverWith{ case p: ParseError[Char] => 
reset(pos0_235)
Failure( p  )
}
}


def OPEN(): Try[PTree] = {
val pos0_240 = mark
val res_241 = for{
catPart_242 <- expect('(').map{ char_244 => PLeaf(char_244.toString) }
catPart_243 <- Spacing()
 }  yield PBranch("OPEN",Seq( catPart_242,catPart_243 )) 
res_241.recoverWith{ case p: ParseError[Char] => 
reset(pos0_240)
Failure( p  )
}
}


def CLOSE(): Try[PTree] = {
val pos0_245 = mark
val res_246 = for{
catPart_247 <- expect(')').map{ char_249 => PLeaf(char_249.toString) }
catPart_248 <- Spacing()
 }  yield PBranch("CLOSE",Seq( catPart_247,catPart_248 )) 
res_246.recoverWith{ case p: ParseError[Char] => 
reset(pos0_245)
Failure( p  )
}
}


def DOT(): Try[PTree] = {
val pos0_250 = mark
val res_251 = for{
catPart_252 <- expect('.').map{ char_254 => PLeaf(char_254.toString) }
catPart_253 <- Spacing()
 }  yield PBranch("DOT",Seq( catPart_252,catPart_253 )) 
res_251.recoverWith{ case p: ParseError[Char] => 
reset(pos0_250)
Failure( p  )
}
}


def Spacing(): Try[PTree] = {
def subMatch_258 = {
val pos_259 = mark
Space().recoverWith{ case err_260: ParseError[Char] =>
reset(pos_259)
Comment().recoverWith{ case err_261: ParseError[Char] =>
reset(pos_259)
Failure(err_260 ~ err_261 ~  ParseFailed("",pos_259) )
}
}
}
var buf_255 = ArrayBuffer.empty[PTree]
var pos_256 = mark
var res_257 = subMatch_258
res_257.recover{ _ => reset(pos_256) }
while(res_257.isSuccess){
buf_255 += res_257.get
pos_256 = mark
res_257 = subMatch_258
res_257.recover{ _ => reset(pos_256) }
}
 Try(PBranch("Spacing",buf_255.toSeq)) 
}


def Comment(): Try[PTree] = {
val pos0_262 = mark
val res_263 = for{
catPart_264 <- expect('#').map{ char_267 => PLeaf(char_267.toString) }
catPart_265 <- {
def subMatch_271 = {
val pos0_272 = mark
val res_273 = for{
catPart_274 <- {
val pos_276 = mark
val res_277 = EndOfLine()
reset(pos_276)
if( res_277.isSuccess ) Failure(ParseFailed("Neglook failed",pos_276))
 else { Try(PEmpty) }
}
catPart_275 <- any.map{ char_278 => PLeaf(char_278.toString)}
 }  yield PBranch("catPart_265",Seq( catPart_274,catPart_275 )) 
res_273.recoverWith{ case p: ParseError[Char] => 
reset(pos0_272)
Failure( p  )
}
}
var buf_268 = ArrayBuffer.empty[PTree]
var pos_269 = mark
var res_270 = subMatch_271
res_270.recover{ _ => reset(pos_269) }
while(res_270.isSuccess){
buf_268 += res_270.get
pos_269 = mark
res_270 = subMatch_271
res_270.recover{ _ => reset(pos_269) }
}
 Try(PBranch("catPart_265",buf_268.toSeq)) 
}
catPart_266 <- EndOfLine()
 }  yield PBranch("Comment",Seq( catPart_264,catPart_265,catPart_266 )) 
res_263.recoverWith{ case p: ParseError[Char] => 
reset(pos0_262)
Failure( p  )
}
}


def Space(): Try[PTree] = {
val pos_279 = mark
expect(' ').map{ char_280 => PLeaf(char_280.toString) }
.recoverWith{ case err_281: ParseError[Char] => 
reset(pos_279)
expect('\t').map{ char_282 => PLeaf(char_282.toString) }
.recoverWith{ case err_283: ParseError[Char] => 
reset(pos_279)
EndOfLine().recoverWith{ case err_284: ParseError[Char] =>
reset(pos_279)
Failure(err_281 ~ err_283 ~ err_284 ~  ParseFailed("",pos_279) )
}
}
}
}


def EndOfLine(): Try[PTree] = {
val pos_285 = mark
val res_286 = {
val pos_288= mark
val res_289 = for{
char_part_290 <- expect('\r')
char_part_291 <- expect('\n')
 } yield PBranch("Lit",Seq( PLeaf(char_part_290.toString) , PLeaf(char_part_291.toString) )) 
res_289.recoverWith{ case p: ParseError[Char] =>
reset(pos_288)
 Failure( p ~ ParseFailed("expected '\r','\n'",pos_288) ) 
}
}
res_286.recoverWith{ case err_287: ParseError[Char] =>
reset(pos_285)
expect('\n').map{ char_292 => PLeaf(char_292.toString) }
.recoverWith{ case err_293: ParseError[Char] => 
reset(pos_285)
expect('\r').map{ char_294 => PLeaf(char_294.toString) }
.recoverWith{ case err_295: ParseError[Char] => 
reset(pos_285)
Failure(err_287 ~ err_293 ~ err_295 ~  ParseFailed("",pos_285) )
}
}
}
}


def EndOfFile(): Try[PTree] = {
val pos_296 = mark
val res_297 = {
val pos_298 = mark
any.map{ x => PLeaf(x.toString)  }
.recoverWith{ case p: ParseError[Char] =>
reset(pos_298)
 Failure( p ~ ParseFailed("Expected any char",pos_298) )  
}
}
reset(pos_296)
if( res_297.isSuccess ) Failure(ParseFailed("Neglook failed",pos_296))
 else { Try(PEmpty) }
}
}
