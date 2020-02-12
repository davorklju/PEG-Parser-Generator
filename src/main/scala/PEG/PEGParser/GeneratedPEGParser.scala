package PEG.PEGParser

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
import PEG.lexparse.ParseError.implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer) {


  def Grammar(): Try[PTree] = {
    val pos0_1 = mark
    val res_2 = for {
      catPart_3 <- Spacing()
      catPart_4 <- {
        val pos0_6 = mark
        val res_7 = for {
          catPart_8 <- Definition()
          catPart_9 <- {
            def subMatch_13 = Definition()

            var buf_10 = ArrayBuffer.empty[PTree]
            var pos_11 = mark
            var res_12 = subMatch_13
            res_12.recover { _ => reset(pos_11) }
            while (res_12.isSuccess) {
              buf_10 += res_12.get
              pos_11 = mark
              res_12 = subMatch_13
              res_12.recover { _ => reset(pos_11) }
            }
            Try(PBranch("catPart_9", buf_10.toSeq))
          }
        } yield PBranch("catPart_4", Seq(catPart_8, catPart_9))
        res_7.recoverWith { case p: ParseError[Char] =>
          reset(pos0_6)
          Failure(p)
        }
      }
      catPart_5 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_3, catPart_4, catPart_5))
    res_2.recoverWith { case p: ParseError[Char] =>
      reset(pos0_1)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_14 = mark
    val res_15 = for {
      catPart_16 <- Identifier()
      catPart_17 <- {
        val pos_20 = mark
        STAR().recoverWith { case err_21: ParseError[Char] =>
          reset(pos_20)
          Try(PEmpty)
        }
      }
      catPart_18 <- LEFTARROW()
      catPart_19 <- ExprWithAction()
    } yield PBranch("Definition", Seq(catPart_16, catPart_17, catPart_18, catPart_19))
    res_15.recoverWith { case p: ParseError[Char] =>
      reset(pos0_14)
      Failure(p)
    }
  }


  def ExprWithAction(): Try[PTree] = {
    val pos0_22 = mark
    val res_23 = for {
      catPart_24 <- ExprPart()
      catPart_25 <- {
        def subMatch_29 = {
          val pos0_30 = mark
          val res_31 = for {
            catPart_32 <- SLASH()
            catPart_33 <- ExprPart()
          } yield PBranch("catPart_25", Seq(catPart_32, catPart_33))
          res_31.recoverWith { case p: ParseError[Char] =>
            reset(pos0_30)
            Failure(p)
          }
        }

        var buf_26 = ArrayBuffer.empty[PTree]
        var pos_27 = mark
        var res_28 = subMatch_29
        res_28.recover { _ => reset(pos_27) }
        while (res_28.isSuccess) {
          buf_26 += res_28.get
          pos_27 = mark
          res_28 = subMatch_29
          res_28.recover { _ => reset(pos_27) }
        }
        Try(PBranch("catPart_25", buf_26.toSeq))
      }
    } yield PBranch("ExprWithAction", Seq(catPart_24, catPart_25))
    res_23.recoverWith { case p: ParseError[Char] =>
      reset(pos0_22)
      Failure(p)
    }
  }


  def ExprPart(): Try[PTree] = {
    val pos0_34 = mark
    val res_35 = for {
      catPart_36 <- Sequence()
      catPart_37 <- {
        val pos_38 = mark
        Action().recoverWith { case err_39: ParseError[Char] =>
          reset(pos_38)
          Try(PEmpty)
        }
      }
    } yield PBranch("ExprPart", Seq(catPart_36, catPart_37))
    res_35.recoverWith { case p: ParseError[Char] =>
      reset(pos0_34)
      Failure(p)
    }
  }


  def Action(): Try[PTree] = {
    val pos0_40 = mark
    val res_41 = for {
      catPart_42 <- OPENCURLY()
      catPart_43 <- {
        val pos0_49 = mark
        val res_50 = for {
          catPart_51 <- {
            val pos0_53 = mark
            val res_54 = for {
              catPart_55 <- {
                val pos_57 = mark
                val res_58 = expect('|', '}')
                reset(pos_57)
                if (res_58.isSuccess) Failure(ParseFailed("Neglook failed", pos_57))
                else {
                  Try(PEmpty)
                }
              }
              catPart_56 <- any.map { char_59 => PLeaf(char_59.toString) }
            } yield PBranch("catPart_51", Seq(catPart_55, catPart_56))
            res_54.recoverWith { case p: ParseError[Char] =>
              reset(pos0_53)
              Failure(p)
            }
          }
          catPart_52 <- {
            def subMatch_63 = {
              val pos0_64 = mark
              val res_65 = for {
                catPart_66 <- {
                  val pos_68 = mark
                  val res_69 = expect('|', '}')
                  reset(pos_68)
                  if (res_69.isSuccess) Failure(ParseFailed("Neglook failed", pos_68))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_67 <- any.map { char_70 => PLeaf(char_70.toString) }
              } yield PBranch("catPart_52", Seq(catPart_66, catPart_67))
              res_65.recoverWith { case p: ParseError[Char] =>
                reset(pos0_64)
                Failure(p)
              }
            }

            var buf_60 = ArrayBuffer.empty[PTree]
            var pos_61 = mark
            var res_62 = subMatch_63
            res_62.recover { _ => reset(pos_61) }
            while (res_62.isSuccess) {
              buf_60 += res_62.get
              pos_61 = mark
              res_62 = subMatch_63
              res_62.recover { _ => reset(pos_61) }
            }
            Try(PBranch("catPart_52", buf_60.toSeq))
          }
        } yield PBranch("catPart_43", Seq(catPart_51, catPart_52))
        res_50.recoverWith { case p: ParseError[Char] =>
          reset(pos0_49)
          Failure(p)
        }
      }
      catPart_44 <- VERTBAR()
      catPart_45 <- {
        def subMatch_74 = Identifier()

        var buf_71 = ArrayBuffer.empty[PTree]
        var pos_72 = mark
        var res_73 = subMatch_74
        res_73.recover { _ => reset(pos_72) }
        while (res_73.isSuccess) {
          buf_71 += res_73.get
          pos_72 = mark
          res_73 = subMatch_74
          res_73.recover { _ => reset(pos_72) }
        }
        Try(PBranch("catPart_45", buf_71.toSeq))
      }
      catPart_46 <- VERTBAR()
      catPart_47 <- {
        def subMatch_78 = {
          val pos0_79 = mark
          val res_80 = for {
            catPart_81 <- {
              val pos_83 = mark
              val res_84 = expect('}')
              reset(pos_83)
              if (res_84.isSuccess) Failure(ParseFailed("Neglook failed", pos_83))
              else {
                Try(PEmpty)
              }
            }
            catPart_82 <- any.map { char_85 => PLeaf(char_85.toString) }
          } yield PBranch("catPart_47", Seq(catPart_81, catPart_82))
          res_80.recoverWith { case p: ParseError[Char] =>
            reset(pos0_79)
            Failure(p)
          }
        }

        var buf_75 = ArrayBuffer.empty[PTree]
        var pos_76 = mark
        var res_77 = subMatch_78
        res_77.recover { _ => reset(pos_76) }
        while (res_77.isSuccess) {
          buf_75 += res_77.get
          pos_76 = mark
          res_77 = subMatch_78
          res_77.recover { _ => reset(pos_76) }
        }
        Try(PBranch("catPart_47", buf_75.toSeq))
      }
      catPart_48 <- CLOSECURLY()
    } yield PBranch("Action", Seq(catPart_42, catPart_43, catPart_44, catPart_45, catPart_46, catPart_47, catPart_48))
    res_41.recoverWith { case p: ParseError[Char] =>
      reset(pos0_40)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_86 = mark
    val res_87 = for {
      catPart_88 <- Sequence()
      catPart_89 <- {
        def subMatch_93 = {
          val pos0_94 = mark
          val res_95 = for {
            catPart_96 <- SLASH()
            catPart_97 <- Sequence()
          } yield PBranch("catPart_89", Seq(catPart_96, catPart_97))
          res_95.recoverWith { case p: ParseError[Char] =>
            reset(pos0_94)
            Failure(p)
          }
        }

        var buf_90 = ArrayBuffer.empty[PTree]
        var pos_91 = mark
        var res_92 = subMatch_93
        res_92.recover { _ => reset(pos_91) }
        while (res_92.isSuccess) {
          buf_90 += res_92.get
          pos_91 = mark
          res_92 = subMatch_93
          res_92.recover { _ => reset(pos_91) }
        }
        Try(PBranch("catPart_89", buf_90.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_88, catPart_89))
    res_87.recoverWith { case p: ParseError[Char] =>
      reset(pos0_86)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def subMatch_101 = Prefix()

    var buf_98 = ArrayBuffer.empty[PTree]
    var pos_99 = mark
    var res_100 = subMatch_101
    res_100.recover { _ => reset(pos_99) }
    while (res_100.isSuccess) {
      buf_98 += res_100.get
      pos_99 = mark
      res_100 = subMatch_101
      res_100.recover { _ => reset(pos_99) }
    }
    Try(PBranch("Sequence", buf_98.toSeq))
  }


  def Prefix(): Try[PTree] = {
    val pos0_102 = mark
    val res_103 = for {
      catPart_104 <- {
        val pos_106 = mark
        val res_107 = {
          val pos_109 = mark
          AND().recoverWith { case err_110: ParseError[Char] =>
            reset(pos_109)
            NOT().recoverWith { case err_111: ParseError[Char] =>
              reset(pos_109)
              Failure(err_110 ~ err_111 ~ ParseFailed("", pos_109))
            }
          }
        }
        res_107.recoverWith { case err_108: ParseError[Char] =>
          reset(pos_106)
          Try(PEmpty)
        }
      }
      catPart_105 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_104, catPart_105))
    res_103.recoverWith { case p: ParseError[Char] =>
      reset(pos0_102)
      Failure(p)
    }
  }


  def Suffix(): Try[PTree] = {
    val pos0_112 = mark
    val res_113 = for {
      catPart_114 <- Primary()
      catPart_115 <- {
        val pos_116 = mark
        val res_117 = {
          val pos_119 = mark
          QUESTION().recoverWith { case err_120: ParseError[Char] =>
            reset(pos_119)
            STAR().recoverWith { case err_121: ParseError[Char] =>
              reset(pos_119)
              PLUS().recoverWith { case err_122: ParseError[Char] =>
                reset(pos_119)
                Failure(err_120 ~ err_121 ~ err_122 ~ ParseFailed("", pos_119))
              }
            }
          }
        }
        res_117.recoverWith { case err_118: ParseError[Char] =>
          reset(pos_116)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_114, catPart_115))
    res_113.recoverWith { case p: ParseError[Char] =>
      reset(pos0_112)
      Failure(p)
    }
  }


  val cache_124 = mutable.HashMap.empty[Int, (Try[PTree], Int)]

  def Primary(): Try[PTree] = {
    def parser_123(): Try[PTree] = {
      val pos_128 = mark
      val res_129 = {
        val pos0_131 = mark
        val res_132 = for {
          catPart_133 <- Identifier()
          catPart_134 <- {
            val pos_135 = mark
            val res_136 = {
              val pos0_137 = mark
              val res_138 = for {
                catPart_139 <- {
                  val pos_141 = mark
                  STAR().recoverWith { case err_142: ParseError[Char] =>
                    reset(pos_141)
                    Try(PEmpty)
                  }
                }
                catPart_140 <- LEFTARROW()
              } yield PBranch("catPart_134", Seq(catPart_139, catPart_140))
              res_138.recoverWith { case p: ParseError[Char] =>
                reset(pos0_137)
                Failure(p)
              }
            }
            reset(pos_135)
            if (res_136.isSuccess) Failure(ParseFailed("Neglook failed", pos_135))
            else {
              Try(PEmpty)
            }
          }
        } yield PBranch("Primary", Seq(catPart_133, catPart_134))
        res_132.recoverWith { case p: ParseError[Char] =>
          reset(pos0_131)
          Failure(p)
        }
      }
      res_129.recoverWith { case err_130: ParseError[Char] =>
        reset(pos_128)
        val res_143 = {
          val pos0_145 = mark
          val res_146 = for {
            catPart_147 <- OPEN()
            catPart_148 <- Expression()
            catPart_149 <- CLOSE()
          } yield PBranch("Primary", Seq(catPart_147, catPart_148, catPart_149))
          res_146.recoverWith { case p: ParseError[Char] =>
            reset(pos0_145)
            Failure(p)
          }
        }
        res_143.recoverWith { case err_144: ParseError[Char] =>
          reset(pos_128)
          Literal().recoverWith { case err_150: ParseError[Char] =>
            reset(pos_128)
            Class().recoverWith { case err_151: ParseError[Char] =>
              reset(pos_128)
              DOT().recoverWith { case err_152: ParseError[Char] =>
                reset(pos_128)
                Failure(err_130 ~ err_144 ~ err_150 ~ err_151 ~ err_152 ~ ParseFailed("", pos_128))
              }
            }
          }
        }
      }
    }

    if (!cache_124.contains(mark)) {
      val init_126 = mark
      cache_124(init_126) = parser_123() -> mark
      reset(init_126)
    }
    val (res_125, pos_127) = cache_124(mark)
    reset(pos_127)
    res_125
  }


  def Identifier(): Try[PTree] = {
    val pos0_153 = mark
    val res_154 = for {
      catPart_155 <- IdentStart()
      catPart_156 <- {
        def subMatch_161 = IdentCont()

        var buf_158 = ArrayBuffer.empty[PTree]
        var pos_159 = mark
        var res_160 = subMatch_161
        res_160.recover { _ => reset(pos_159) }
        while (res_160.isSuccess) {
          buf_158 += res_160.get
          pos_159 = mark
          res_160 = subMatch_161
          res_160.recover { _ => reset(pos_159) }
        }
        Try(PBranch("catPart_156", buf_158.toSeq))
      }
      catPart_157 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_155, catPart_156, catPart_157))
    res_154.recoverWith { case p: ParseError[Char] =>
      reset(pos0_153)
      Failure(p)
    }
  }


  def IdentStart(): Try[PTree] = {
    val pos_162 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_163 => PLeaf(char_163.toString) }
      .recoverWith { case p: ParseError[Char] =>
        reset(pos_162)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_162))
      }
  }


  def IdentCont(): Try[PTree] = {
    val pos_164 = mark
    IdentStart().recoverWith { case err_165: ParseError[Char] =>
      reset(pos_164)
      expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
        .map { char_166 => PLeaf(char_166.toString) }
        .recoverWith { case err_167: ParseError[Char] =>
          reset(pos_164)
          Failure(err_165 ~ err_167 ~ ParseFailed("", pos_164))
        }
    }
  }


  def Literal(): Try[PTree] = {
    val pos_168 = mark
    val res_169 = {
      val pos0_171 = mark
      val res_172 = for {
        catPart_173 <- expect('\'').map { char_177 => PLeaf(char_177.toString) }
        catPart_174 <- {
          def subMatch_181 = {
            val pos0_182 = mark
            val res_183 = for {
              catPart_184 <- {
                val pos_186 = mark
                val res_187 = expect('\'')
                reset(pos_186)
                if (res_187.isSuccess) Failure(ParseFailed("Neglook failed", pos_186))
                else {
                  Try(PEmpty)
                }
              }
              catPart_185 <- Char()
            } yield PBranch("catPart_174", Seq(catPart_184, catPart_185))
            res_183.recoverWith { case p: ParseError[Char] =>
              reset(pos0_182)
              Failure(p)
            }
          }

          var buf_178 = ArrayBuffer.empty[PTree]
          var pos_179 = mark
          var res_180 = subMatch_181
          res_180.recover { _ => reset(pos_179) }
          while (res_180.isSuccess) {
            buf_178 += res_180.get
            pos_179 = mark
            res_180 = subMatch_181
            res_180.recover { _ => reset(pos_179) }
          }
          Try(PBranch("catPart_174", buf_178.toSeq))
        }
        catPart_175 <- expect('\'').map { char_188 => PLeaf(char_188.toString) }
        catPart_176 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_173, catPart_174, catPart_175, catPart_176))
      res_172.recoverWith { case p: ParseError[Char] =>
        reset(pos0_171)
        Failure(p)
      }
    }
    res_169.recoverWith { case err_170: ParseError[Char] =>
      reset(pos_168)
      val res_189 = {
        val pos0_191 = mark
        val res_192 = for {
          catPart_193 <- expect('"').map { char_197 => PLeaf(char_197.toString) }
          catPart_194 <- {
            def subMatch_201 = {
              val pos0_202 = mark
              val res_203 = for {
                catPart_204 <- {
                  val pos_206 = mark
                  val res_207 = expect('"')
                  reset(pos_206)
                  if (res_207.isSuccess) Failure(ParseFailed("Neglook failed", pos_206))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_205 <- Char()
              } yield PBranch("catPart_194", Seq(catPart_204, catPart_205))
              res_203.recoverWith { case p: ParseError[Char] =>
                reset(pos0_202)
                Failure(p)
              }
            }

            var buf_198 = ArrayBuffer.empty[PTree]
            var pos_199 = mark
            var res_200 = subMatch_201
            res_200.recover { _ => reset(pos_199) }
            while (res_200.isSuccess) {
              buf_198 += res_200.get
              pos_199 = mark
              res_200 = subMatch_201
              res_200.recover { _ => reset(pos_199) }
            }
            Try(PBranch("catPart_194", buf_198.toSeq))
          }
          catPart_195 <- expect('"').map { char_208 => PLeaf(char_208.toString) }
          catPart_196 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_193, catPart_194, catPart_195, catPart_196))
        res_192.recoverWith { case p: ParseError[Char] =>
          reset(pos0_191)
          Failure(p)
        }
      }
      res_189.recoverWith { case err_190: ParseError[Char] =>
        reset(pos_168)
        val res_209 = {
          val pos0_211 = mark
          val res_212 = for {
            catPart_213 <- expect('`').map { char_217 => PLeaf(char_217.toString) }
            catPart_214 <- {
              def subMatch_221 = {
                val pos0_222 = mark
                val res_223 = for {
                  catPart_224 <- {
                    val pos_226 = mark
                    val res_227 = expect('`')
                    reset(pos_226)
                    if (res_227.isSuccess) Failure(ParseFailed("Neglook failed", pos_226))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_225 <- Char()
                } yield PBranch("catPart_214", Seq(catPart_224, catPart_225))
                res_223.recoverWith { case p: ParseError[Char] =>
                  reset(pos0_222)
                  Failure(p)
                }
              }

              var buf_218 = ArrayBuffer.empty[PTree]
              var pos_219 = mark
              var res_220 = subMatch_221
              res_220.recover { _ => reset(pos_219) }
              while (res_220.isSuccess) {
                buf_218 += res_220.get
                pos_219 = mark
                res_220 = subMatch_221
                res_220.recover { _ => reset(pos_219) }
              }
              Try(PBranch("catPart_214", buf_218.toSeq))
            }
            catPart_215 <- expect('`').map { char_228 => PLeaf(char_228.toString) }
            catPart_216 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_213, catPart_214, catPart_215, catPart_216))
          res_212.recoverWith { case p: ParseError[Char] =>
            reset(pos0_211)
            Failure(p)
          }
        }
        res_209.recoverWith { case err_210: ParseError[Char] =>
          reset(pos_168)
          Failure(err_170 ~ err_190 ~ err_210 ~ ParseFailed("", pos_168))
        }
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_229 = mark
    val res_230 = for {
      catPart_231 <- expect('[').map { char_235 => PLeaf(char_235.toString) }
      catPart_232 <- {
        def subMatch_239 = {
          val pos0_240 = mark
          val res_241 = for {
            catPart_242 <- {
              val pos_244 = mark
              val res_245 = {
                val pos_246 = mark
                val res_247 = for {
                  char_part_248 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_248.toString)))
                res_247.recoverWith { case p: ParseError[Char] =>
                  reset(pos_246)
                  Failure(p ~ ParseFailed("expected ']'", pos_246))
                }
              }
              reset(pos_244)
              if (res_245.isSuccess) Failure(ParseFailed("Neglook failed", pos_244))
              else {
                Try(PEmpty)
              }
            }
            catPart_243 <- Range()
          } yield PBranch("catPart_232", Seq(catPart_242, catPart_243))
          res_241.recoverWith { case p: ParseError[Char] =>
            reset(pos0_240)
            Failure(p)
          }
        }

        var buf_236 = ArrayBuffer.empty[PTree]
        var pos_237 = mark
        var res_238 = subMatch_239
        res_238.recover { _ => reset(pos_237) }
        while (res_238.isSuccess) {
          buf_236 += res_238.get
          pos_237 = mark
          res_238 = subMatch_239
          res_238.recover { _ => reset(pos_237) }
        }
        Try(PBranch("catPart_232", buf_236.toSeq))
      }
      catPart_233 <- expect(']').map { char_249 => PLeaf(char_249.toString) }
      catPart_234 <- Spacing()
    } yield PBranch("Class", Seq(catPart_231, catPart_232, catPart_233, catPart_234))
    res_230.recoverWith { case p: ParseError[Char] =>
      reset(pos0_229)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_250 = mark
    val res_251 = {
      val pos0_253 = mark
      val res_254 = for {
        catPart_255 <- Char()
        catPart_256 <- expect('-').map { char_258 => PLeaf(char_258.toString) }
        catPart_257 <- Char()
      } yield PBranch("Range", Seq(catPart_255, catPart_256, catPart_257))
      res_254.recoverWith { case p: ParseError[Char] =>
        reset(pos0_253)
        Failure(p)
      }
    }
    res_251.recoverWith { case err_252: ParseError[Char] =>
      reset(pos_250)
      Char().recoverWith { case err_259: ParseError[Char] =>
        reset(pos_250)
        Failure(err_252 ~ err_259 ~ ParseFailed("", pos_250))
      }
    }
  }


  def Char(): Try[PTree] = {
    val pos_260 = mark
    val res_261 = {
      val pos0_263 = mark
      val res_264 = for {
        catPart_265 <- expect('\\').map { char_267 => PLeaf(char_267.toString) }
        catPart_266 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"').map { char_268 => PLeaf(char_268.toString) }
      } yield PBranch("Char", Seq(catPart_265, catPart_266))
      res_264.recoverWith { case p: ParseError[Char] =>
        reset(pos0_263)
        Failure(p)
      }
    }
    res_261.recoverWith { case err_262: ParseError[Char] =>
      reset(pos_260)
      val res_269 = {
        val pos0_271 = mark
        val res_272 = for {
          catPart_273 <- {
            val pos_275 = mark
            val res_276 = {
              val pos_277 = mark
              val res_278 = for {
                char_part_279 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_279.toString)))
              res_278.recoverWith { case p: ParseError[Char] =>
                reset(pos_277)
                Failure(p ~ ParseFailed("expected '\\'", pos_277))
              }
            }
            reset(pos_275)
            if (res_276.isSuccess) Failure(ParseFailed("Neglook failed", pos_275))
            else {
              Try(PEmpty)
            }
          }
          catPart_274 <- any.map { char_280 => PLeaf(char_280.toString) }
        } yield PBranch("Char", Seq(catPart_273, catPart_274))
        res_272.recoverWith { case p: ParseError[Char] =>
          reset(pos0_271)
          Failure(p)
        }
      }
      res_269.recoverWith { case err_270: ParseError[Char] =>
        reset(pos_260)
        Failure(err_262 ~ err_270 ~ ParseFailed("", pos_260))
      }
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_281 = mark
    val res_282 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_283 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_284 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_283, catPart_284))
    res_282.recoverWith { case p: ParseError[Char] =>
      reset(pos0_281)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_285 = mark
    val res_286 = for {
      catPart_287 <- expect('/').map { char_289 => PLeaf(char_289.toString) }
      catPart_288 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_287, catPart_288))
    res_286.recoverWith { case p: ParseError[Char] =>
      reset(pos0_285)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_290 = mark
    val res_291 = for {
      catPart_292 <- expect('&').map { char_294 => PLeaf(char_294.toString) }
      catPart_293 <- Spacing()
    } yield PBranch("AND", Seq(catPart_292, catPart_293))
    res_291.recoverWith { case p: ParseError[Char] =>
      reset(pos0_290)
      Failure(p)
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_295 = mark
    val res_296 = for {
      catPart_297 <- expect('!').map { char_299 => PLeaf(char_299.toString) }
      catPart_298 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_297, catPart_298))
    res_296.recoverWith { case p: ParseError[Char] =>
      reset(pos0_295)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_300 = mark
    val res_301 = for {
      catPart_302 <- expect('?').map { char_304 => PLeaf(char_304.toString) }
      catPart_303 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_302, catPart_303))
    res_301.recoverWith { case p: ParseError[Char] =>
      reset(pos0_300)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_305 = mark
    val res_306 = for {
      catPart_307 <- expect('*').map { char_309 => PLeaf(char_309.toString) }
      catPart_308 <- Spacing()
    } yield PBranch("STAR", Seq(catPart_307, catPart_308))
    res_306.recoverWith { case p: ParseError[Char] =>
      reset(pos0_305)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_310 = mark
    val res_311 = for {
      catPart_312 <- expect('+').map { char_314 => PLeaf(char_314.toString) }
      catPart_313 <- Spacing()
    } yield PBranch("PLUS", Seq(catPart_312, catPart_313))
    res_311.recoverWith { case p: ParseError[Char] =>
      reset(pos0_310)
      Failure(p)
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_315 = mark
    val res_316 = for {
      catPart_317 <- expect('(').map { char_319 => PLeaf(char_319.toString) }
      catPart_318 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_317, catPart_318))
    res_316.recoverWith { case p: ParseError[Char] =>
      reset(pos0_315)
      Failure(p)
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_320 = mark
    val res_321 = for {
      catPart_322 <- expect(')').map { char_324 => PLeaf(char_324.toString) }
      catPart_323 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_322, catPart_323))
    res_321.recoverWith { case p: ParseError[Char] =>
      reset(pos0_320)
      Failure(p)
    }
  }


  def DOT(): Try[PTree] = {
    val pos0_325 = mark
    val res_326 = for {
      catPart_327 <- expect('.').map { char_329 => PLeaf(char_329.toString) }
      catPart_328 <- Spacing()
    } yield PBranch("DOT", Seq(catPart_327, catPart_328))
    res_326.recoverWith { case p: ParseError[Char] =>
      reset(pos0_325)
      Failure(p)
    }
  }


  def OPENCURLY(): Try[PTree] = {
    val pos0_330 = mark
    val res_331 = for {
      catPart_332 <- expect('{').map { char_334 => PLeaf(char_334.toString) }
      catPart_333 <- Spacing()
    } yield PBranch("OPENCURLY", Seq(catPart_332, catPart_333))
    res_331.recoverWith { case p: ParseError[Char] =>
      reset(pos0_330)
      Failure(p)
    }
  }


  def CLOSECURLY(): Try[PTree] = {
    val pos0_335 = mark
    val res_336 = for {
      catPart_337 <- expect('}').map { char_339 => PLeaf(char_339.toString) }
      catPart_338 <- Spacing()
    } yield PBranch("CLOSECURLY", Seq(catPart_337, catPart_338))
    res_336.recoverWith { case p: ParseError[Char] =>
      reset(pos0_335)
      Failure(p)
    }
  }


  def VERTBAR(): Try[PTree] = {
    val pos0_340 = mark
    val res_341 = for {
      catPart_342 <- expect('|').map { char_344 => PLeaf(char_344.toString) }
      catPart_343 <- Spacing()
    } yield PBranch("VERTBAR", Seq(catPart_342, catPart_343))
    res_341.recoverWith { case p: ParseError[Char] =>
      reset(pos0_340)
      Failure(p)
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_348 = {
      val pos_349 = mark
      Space().recoverWith { case err_350: ParseError[Char] =>
        reset(pos_349)
        Comment().recoverWith { case err_351: ParseError[Char] =>
          reset(pos_349)
          Failure(err_350 ~ err_351 ~ ParseFailed("", pos_349))
        }
      }
    }

    var buf_345 = ArrayBuffer.empty[PTree]
    var pos_346 = mark
    var res_347 = subMatch_348
    res_347.recover { _ => reset(pos_346) }
    while (res_347.isSuccess) {
      buf_345 += res_347.get
      pos_346 = mark
      res_347 = subMatch_348
      res_347.recover { _ => reset(pos_346) }
    }
    Try(PBranch("Spacing", buf_345.toSeq))
  }


  def Comment(): Try[PTree] = {
    val pos0_352 = mark
    val res_353 = for {
      catPart_354 <- expect('#').map { char_357 => PLeaf(char_357.toString) }
      catPart_355 <- {
        def subMatch_361 = {
          val pos0_362 = mark
          val res_363 = for {
            catPart_364 <- {
              val pos_366 = mark
              val res_367 = EndOfLine()
              reset(pos_366)
              if (res_367.isSuccess) Failure(ParseFailed("Neglook failed", pos_366))
              else {
                Try(PEmpty)
              }
            }
            catPart_365 <- any.map { char_368 => PLeaf(char_368.toString) }
          } yield PBranch("catPart_355", Seq(catPart_364, catPart_365))
          res_363.recoverWith { case p: ParseError[Char] =>
            reset(pos0_362)
            Failure(p)
          }
        }

        var buf_358 = ArrayBuffer.empty[PTree]
        var pos_359 = mark
        var res_360 = subMatch_361
        res_360.recover { _ => reset(pos_359) }
        while (res_360.isSuccess) {
          buf_358 += res_360.get
          pos_359 = mark
          res_360 = subMatch_361
          res_360.recover { _ => reset(pos_359) }
        }
        Try(PBranch("catPart_355", buf_358.toSeq))
      }
      catPart_356 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_354, catPart_355, catPart_356))
    res_353.recoverWith { case p: ParseError[Char] =>
      reset(pos0_352)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_369 = mark
    expect(' ').map { char_370 => PLeaf(char_370.toString) }
      .recoverWith { case err_371: ParseError[Char] =>
        reset(pos_369)
        expect('\t').map { char_372 => PLeaf(char_372.toString) }
          .recoverWith { case err_373: ParseError[Char] =>
            reset(pos_369)
            EndOfLine().recoverWith { case err_374: ParseError[Char] =>
              reset(pos_369)
              Failure(err_371 ~ err_373 ~ err_374 ~ ParseFailed("", pos_369))
            }
          }
      }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_375 = mark
    val res_376 = {
      val pos_378 = mark
      val res_379 = for {
        char_part_380 <- expect('\r')
        char_part_381 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_380.toString), PLeaf(char_part_381.toString)))
      res_379.recoverWith { case p: ParseError[Char] =>
        reset(pos_378)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_378))
      }
    }
    res_376.recoverWith { case err_377: ParseError[Char] =>
      reset(pos_375)
      expect('\n').map { char_382 => PLeaf(char_382.toString) }
        .recoverWith { case err_383: ParseError[Char] =>
          reset(pos_375)
          expect('\r').map { char_384 => PLeaf(char_384.toString) }
            .recoverWith { case err_385: ParseError[Char] =>
              reset(pos_375)
              Failure(err_377 ~ err_383 ~ err_385 ~ ParseFailed("", pos_375))
            }
        }
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_386 = mark
    val res_387 = {
      val pos_388 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_388)
          Failure(p ~ ParseFailed("Expected any char", pos_388))
        }
    }
    reset(pos_386)
    if (res_387.isSuccess) Failure(ParseFailed("Neglook failed", pos_386))
    else {
      Try(PEmpty)
    }
  }
}
