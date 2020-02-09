package PEG.PEGParser

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer) {


  def DOT(): Try[PTree] = {
    val pos0_1 = mark
    val res_2 = for {
      catPart_3 <- expect('.').map { char_5 => PLeaf(char_5.toString) }
      catPart_4 <- Spacing()
    } yield PBranch("DOT", Seq(catPart_3, catPart_4))
    res_2.recoverWith { case p: ParseError =>
      reset(pos0_1)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_6 = mark
    val res_7 = for {
      catPart_8 <- expect('*').map { char_10 => PLeaf(char_10.toString) }
      catPart_9 <- Spacing()
    } yield PBranch("STAR", Seq(catPart_8, catPart_9))
    res_7.recoverWith { case p: ParseError =>
      reset(pos0_6)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_11 = mark
    val res_12 = for {
      catPart_13 <- expect('+').map { char_15 => PLeaf(char_15.toString) }
      catPart_14 <- Spacing()
    } yield PBranch("PLUS", Seq(catPart_13, catPart_14))
    res_12.recoverWith { case p: ParseError =>
      reset(pos0_11)
      Failure(p)
    }
  }


  def Literal(): Try[PTree] = {
    val pos_16 = mark
    val res_17 = {
      val pos0_19 = mark
      val res_20 = for {
        catPart_21 <- expect('\'').map { char_25 => PLeaf(char_25.toString) }
        catPart_22 <- {
          def subMatch_29 = {
            val pos0_30 = mark
            val res_31 = for {
              catPart_32 <- {
                val pos_34 = mark
                val res_35 = expect('\'')
                reset(pos_34)
                if (res_35.isSuccess) Failure(ParseFailed("Neglook failed", pos_34))
                else {
                  Try(PEmpty)
                }
              }
              catPart_33 <- Char()
            } yield PBranch("catPart_22", Seq(catPart_32, catPart_33))
            res_31.recoverWith { case p: ParseError =>
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
          Try(PBranch("catPart_22", buf_26.toSeq))
        }
        catPart_23 <- expect('\'').map { char_36 => PLeaf(char_36.toString) }
        catPart_24 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_21, catPart_22, catPart_23, catPart_24))
      res_20.recoverWith { case p: ParseError =>
        reset(pos0_19)
        Failure(p)
      }
    }
    res_17.recoverWith { case err_18: ParseError =>
      reset(pos_16)
      val res_37 = {
        val pos0_39 = mark
        val res_40 = for {
          catPart_41 <- expect('"').map { char_45 => PLeaf(char_45.toString) }
          catPart_42 <- {
            def subMatch_49 = {
              val pos0_50 = mark
              val res_51 = for {
                catPart_52 <- {
                  val pos_54 = mark
                  val res_55 = expect('"')
                  reset(pos_54)
                  if (res_55.isSuccess) Failure(ParseFailed("Neglook failed", pos_54))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_53 <- Char()
              } yield PBranch("catPart_42", Seq(catPart_52, catPart_53))
              res_51.recoverWith { case p: ParseError =>
                reset(pos0_50)
                Failure(p)
              }
            }

            var buf_46 = ArrayBuffer.empty[PTree]
            var pos_47 = mark
            var res_48 = subMatch_49
            res_48.recover { _ => reset(pos_47) }
            while (res_48.isSuccess) {
              buf_46 += res_48.get
              pos_47 = mark
              res_48 = subMatch_49
              res_48.recover { _ => reset(pos_47) }
            }
            Try(PBranch("catPart_42", buf_46.toSeq))
          }
          catPart_43 <- expect('"').map { char_56 => PLeaf(char_56.toString) }
          catPart_44 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_41, catPart_42, catPart_43, catPart_44))
        res_40.recoverWith { case p: ParseError =>
          reset(pos0_39)
          Failure(p)
        }
      }
      res_37.recoverWith { case err_38: ParseError =>
        reset(pos_16)
        val res_57 = {
          val pos0_59 = mark
          val res_60 = for {
            catPart_61 <- expect('`').map { char_65 => PLeaf(char_65.toString) }
            catPart_62 <- {
              def subMatch_69 = {
                val pos0_70 = mark
                val res_71 = for {
                  catPart_72 <- {
                    val pos_74 = mark
                    val res_75 = expect('`')
                    reset(pos_74)
                    if (res_75.isSuccess) Failure(ParseFailed("Neglook failed", pos_74))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_73 <- Char()
                } yield PBranch("catPart_62", Seq(catPart_72, catPart_73))
                res_71.recoverWith { case p: ParseError =>
                  reset(pos0_70)
                  Failure(p)
                }
              }

              var buf_66 = ArrayBuffer.empty[PTree]
              var pos_67 = mark
              var res_68 = subMatch_69
              res_68.recover { _ => reset(pos_67) }
              while (res_68.isSuccess) {
                buf_66 += res_68.get
                pos_67 = mark
                res_68 = subMatch_69
                res_68.recover { _ => reset(pos_67) }
              }
              Try(PBranch("catPart_62", buf_66.toSeq))
            }
            catPart_63 <- expect('`').map { char_76 => PLeaf(char_76.toString) }
            catPart_64 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_61, catPart_62, catPart_63, catPart_64))
          res_60.recoverWith { case p: ParseError =>
            reset(pos0_59)
            Failure(p)
          }
        }
        res_57.recoverWith { case err_58: ParseError =>
          reset(pos_16)
          Failure(err_18 ~ err_38 ~ err_58 ~ ParseFailed("", pos_16))
        }
      }
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_77 = mark
    val res_78 = for {
      catPart_79 <- expect(')').map { char_81 => PLeaf(char_81.toString) }
      catPart_80 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_79, catPart_80))
    res_78.recoverWith { case p: ParseError =>
      reset(pos0_77)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_82 = mark
    val res_83 = for {
      catPart_84 <- Identifier()
      catPart_85 <- LEFTARROW()
      catPart_86 <- Expression()
    } yield PBranch("Definition", Seq(catPart_84, catPart_85, catPart_86))
    res_83.recoverWith { case p: ParseError =>
      reset(pos0_82)
      Failure(p)
    }
  }


  def Char(): Try[PTree] = {
    val pos_87 = mark
    val res_88 = {
      val pos0_90 = mark
      val res_91 = for {
        catPart_92 <- expect('\\').map { char_94 => PLeaf(char_94.toString) }
        catPart_93 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"').map { char_95 => PLeaf(char_95.toString) }
      } yield PBranch("Char", Seq(catPart_92, catPart_93))
      res_91.recoverWith { case p: ParseError =>
        reset(pos0_90)
        Failure(p)
      }
    }
    res_88.recoverWith { case err_89: ParseError =>
      reset(pos_87)
      val res_96 = {
        val pos0_98 = mark
        val res_99 = for {
          catPart_100 <- {
            val pos_102 = mark
            val res_103 = {
              val pos_104 = mark
              val res_105 = for {
                char_part_106 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_106.toString)))
              res_105.recoverWith { case p: ParseError =>
                reset(pos_104)
                Failure(p ~ ParseFailed("expected '\\'", pos_104))
              }
            }
            reset(pos_102)
            if (res_103.isSuccess) Failure(ParseFailed("Neglook failed", pos_102))
            else {
              Try(PEmpty)
            }
          }
          catPart_101 <- any.map { char_107 => PLeaf(char_107.toString) }
        } yield PBranch("Char", Seq(catPart_100, catPart_101))
        res_99.recoverWith { case p: ParseError =>
          reset(pos0_98)
          Failure(p)
        }
      }
      res_96.recoverWith { case err_97: ParseError =>
        reset(pos_87)
        Failure(err_89 ~ err_97 ~ ParseFailed("", pos_87))
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_108 = mark
    val res_109 = for {
      catPart_110 <- expect('[').map { char_114 => PLeaf(char_114.toString) }
      catPart_111 <- {
        def subMatch_118 = {
          val pos0_119 = mark
          val res_120 = for {
            catPart_121 <- {
              val pos_123 = mark
              val res_124 = {
                val pos_125 = mark
                val res_126 = for {
                  char_part_127 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_127.toString)))
                res_126.recoverWith { case p: ParseError =>
                  reset(pos_125)
                  Failure(p ~ ParseFailed("expected ']'", pos_125))
                }
              }
              reset(pos_123)
              if (res_124.isSuccess) Failure(ParseFailed("Neglook failed", pos_123))
              else {
                Try(PEmpty)
              }
            }
            catPart_122 <- Range()
          } yield PBranch("catPart_111", Seq(catPart_121, catPart_122))
          res_120.recoverWith { case p: ParseError =>
            reset(pos0_119)
            Failure(p)
          }
        }

        var buf_115 = ArrayBuffer.empty[PTree]
        var pos_116 = mark
        var res_117 = subMatch_118
        res_117.recover { _ => reset(pos_116) }
        while (res_117.isSuccess) {
          buf_115 += res_117.get
          pos_116 = mark
          res_117 = subMatch_118
          res_117.recover { _ => reset(pos_116) }
        }
        Try(PBranch("catPart_111", buf_115.toSeq))
      }
      catPart_112 <- expect(']').map { char_128 => PLeaf(char_128.toString) }
      catPart_113 <- Spacing()
    } yield PBranch("Class", Seq(catPart_110, catPart_111, catPart_112, catPart_113))
    res_109.recoverWith { case p: ParseError =>
      reset(pos0_108)
      Failure(p)
    }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_129 = mark
    val res_130 = {
      val pos_132 = mark
      val res_133 = for {
        char_part_134 <- expect('\r')
        char_part_135 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_134.toString), PLeaf(char_part_135.toString)))
      res_133.recoverWith { case p: ParseError =>
        reset(pos_132)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_132))
      }
    }
    res_130.recoverWith { case err_131: ParseError =>
      reset(pos_129)
      expect('\n').map { char_136 => PLeaf(char_136.toString) }
        .recoverWith { case err_137: ParseError =>
          reset(pos_129)
          expect('\r').map { char_138 => PLeaf(char_138.toString) }
            .recoverWith { case err_139: ParseError =>
              reset(pos_129)
              Failure(err_131 ~ err_137 ~ err_139 ~ ParseFailed("", pos_129))
            }
        }
    }
  }


  def IdentCont(): Try[PTree] = {
    val pos_140 = mark
    IdentStart().recoverWith { case err_141: ParseError =>
      reset(pos_140)
      val res_142 = {
        val pos_144 = mark
        expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
          .map { char_145 => PLeaf(char_145.toString) }
          .recoverWith { case p: ParseError =>
            reset(pos_144)
            Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_144))
          }
      }
      res_142.recoverWith { case err_143: ParseError =>
        reset(pos_140)
        Failure(err_141 ~ err_143 ~ ParseFailed("", pos_140))
      }
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_146 = mark
    val res_147 = for {
      catPart_148 <- expect('!').map { char_150 => PLeaf(char_150.toString) }
      catPart_149 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_148, catPart_149))
    res_147.recoverWith { case p: ParseError =>
      reset(pos0_146)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_151 = mark
    val res_152 = for {
      catPart_153 <- expect('?').map { char_155 => PLeaf(char_155.toString) }
      catPart_154 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_153, catPart_154))
    res_152.recoverWith { case p: ParseError =>
      reset(pos0_151)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_156 = mark
    val res_157 = for {
      catPart_158 <- Sequence()
      catPart_159 <- {
        def subMatch_163 = {
          val pos0_164 = mark
          val res_165 = for {
            catPart_166 <- SLASH()
            catPart_167 <- Sequence()
          } yield PBranch("catPart_159", Seq(catPart_166, catPart_167))
          res_165.recoverWith { case p: ParseError =>
            reset(pos0_164)
            Failure(p)
          }
        }

        var buf_160 = ArrayBuffer.empty[PTree]
        var pos_161 = mark
        var res_162 = subMatch_163
        res_162.recover { _ => reset(pos_161) }
        while (res_162.isSuccess) {
          buf_160 += res_162.get
          pos_161 = mark
          res_162 = subMatch_163
          res_162.recover { _ => reset(pos_161) }
        }
        Try(PBranch("catPart_159", buf_160.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_158, catPart_159))
    res_157.recoverWith { case p: ParseError =>
      reset(pos0_156)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def subMatch_171 = Prefix()

    var buf_168 = ArrayBuffer.empty[PTree]
    var pos_169 = mark
    var res_170 = subMatch_171
    res_170.recover { _ => reset(pos_169) }
    while (res_170.isSuccess) {
      buf_168 += res_170.get
      pos_169 = mark
      res_170 = subMatch_171
      res_170.recover { _ => reset(pos_169) }
    }
    Try(PBranch("Sequence", buf_168.toSeq))
  }


  def Suffix(): Try[PTree] = {
    val pos0_172 = mark
    val res_173 = for {
      catPart_174 <- Primary()
      catPart_175 <- {
        val pos_176 = mark
        val res_177 = {
          val pos_179 = mark
          QUESTION().recoverWith { case err_180: ParseError =>
            reset(pos_179)
            STAR().recoverWith { case err_181: ParseError =>
              reset(pos_179)
              PLUS().recoverWith { case err_182: ParseError =>
                reset(pos_179)
                Failure(err_180 ~ err_181 ~ err_182 ~ ParseFailed("", pos_179))
              }
            }
          }
        }
        res_177.recoverWith { case err_178: ParseError =>
          reset(pos_176)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_174, catPart_175))
    res_173.recoverWith { case p: ParseError =>
      reset(pos0_172)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_183 = mark
    expect(' ').map { char_184 => PLeaf(char_184.toString) }
      .recoverWith { case err_185: ParseError =>
        reset(pos_183)
        expect('\t').map { char_186 => PLeaf(char_186.toString) }
          .recoverWith { case err_187: ParseError =>
            reset(pos_183)
            EndOfLine().recoverWith { case err_188: ParseError =>
              reset(pos_183)
              Failure(err_185 ~ err_187 ~ err_188 ~ ParseFailed("", pos_183))
            }
          }
      }
  }


  def OPEN(): Try[PTree] = {
    val pos0_189 = mark
    val res_190 = for {
      catPart_191 <- expect('(').map { char_193 => PLeaf(char_193.toString) }
      catPart_192 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_191, catPart_192))
    res_190.recoverWith { case p: ParseError =>
      reset(pos0_189)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_194 = mark
    val res_195 = {
      val pos_196 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_196)
          Failure(p ~ ParseFailed("Expected any char", pos_196))
        }
    }
    reset(pos_194)
    if (res_195.isSuccess) Failure(ParseFailed("Neglook failed", pos_194))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_197 = mark
    val res_198 = {
      val pos0_200 = mark
      val res_201 = for {
        catPart_202 <- Identifier()
        catPart_203 <- {
          val pos_204 = mark
          val res_205 = LEFTARROW()
          reset(pos_204)
          if (res_205.isSuccess) Failure(ParseFailed("Neglook failed", pos_204))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_202, catPart_203))
      res_201.recoverWith { case p: ParseError =>
        reset(pos0_200)
        Failure(p)
      }
    }
    res_198.recoverWith { case err_199: ParseError =>
      reset(pos_197)
      val res_206 = {
        val pos0_208 = mark
        val res_209 = for {
          catPart_210 <- OPEN()
          catPart_211 <- Expression()
          catPart_212 <- CLOSE()
        } yield PBranch("Primary", Seq(catPart_210, catPart_211, catPart_212))
        res_209.recoverWith { case p: ParseError =>
          reset(pos0_208)
          Failure(p)
        }
      }
      res_206.recoverWith { case err_207: ParseError =>
        reset(pos_197)
        Literal().recoverWith { case err_213: ParseError =>
          reset(pos_197)
          Class().recoverWith { case err_214: ParseError =>
            reset(pos_197)
            DOT().recoverWith { case err_215: ParseError =>
              reset(pos_197)
              Failure(err_199 ~ err_207 ~ err_213 ~ err_214 ~ err_215 ~ ParseFailed("", pos_197))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_219 = {
      val pos_220 = mark
      Space().recoverWith { case err_221: ParseError =>
        reset(pos_220)
        Comment().recoverWith { case err_222: ParseError =>
          reset(pos_220)
          Failure(err_221 ~ err_222 ~ ParseFailed("", pos_220))
        }
      }
    }

    var buf_216 = ArrayBuffer.empty[PTree]
    var pos_217 = mark
    var res_218 = subMatch_219
    res_218.recover { _ => reset(pos_217) }
    while (res_218.isSuccess) {
      buf_216 += res_218.get
      pos_217 = mark
      res_218 = subMatch_219
      res_218.recover { _ => reset(pos_217) }
    }
    Try(PBranch("Spacing", buf_216.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_223 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_224 => PLeaf(char_224.toString) }
      .recoverWith { case p: ParseError =>
        reset(pos_223)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_223))
      }
  }


  def Prefix(): Try[PTree] = {
    val pos0_225 = mark
    val res_226 = for {
      catPart_227 <- {
        val pos_229 = mark
        val res_230 = {
          val pos_232 = mark
          AND().recoverWith { case err_233: ParseError =>
            reset(pos_232)
            NOT().recoverWith { case err_234: ParseError =>
              reset(pos_232)
              Failure(err_233 ~ err_234 ~ ParseFailed("", pos_232))
            }
          }
        }
        res_230.recoverWith { case err_231: ParseError =>
          reset(pos_229)
          Try(PEmpty)
        }
      }
      catPart_228 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_227, catPart_228))
    res_226.recoverWith { case p: ParseError =>
      reset(pos0_225)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_235 = mark
    val res_236 = for {
      catPart_237 <- IdentStart()
      catPart_238 <- {
        def subMatch_243 = IdentCont()

        var buf_240 = ArrayBuffer.empty[PTree]
        var pos_241 = mark
        var res_242 = subMatch_243
        res_242.recover { _ => reset(pos_241) }
        while (res_242.isSuccess) {
          buf_240 += res_242.get
          pos_241 = mark
          res_242 = subMatch_243
          res_242.recover { _ => reset(pos_241) }
        }
        Try(PBranch("catPart_238", buf_240.toSeq))
      }
      catPart_239 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_237, catPart_238, catPart_239))
    res_236.recoverWith { case p: ParseError =>
      reset(pos0_235)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_244 = mark
    val res_245 = for {
      catPart_246 <- expect('/').map { char_248 => PLeaf(char_248.toString) }
      catPart_247 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_246, catPart_247))
    res_245.recoverWith { case p: ParseError =>
      reset(pos0_244)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_249 = mark
    val res_250 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_251 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_252 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_251, catPart_252))
    res_250.recoverWith { case p: ParseError =>
      reset(pos0_249)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_253 = mark
    val res_254 = for {
      catPart_255 <- expect('#').map { char_258 => PLeaf(char_258.toString) }
      catPart_256 <- {
        def subMatch_262 = {
          val pos0_263 = mark
          val res_264 = for {
            catPart_265 <- {
              val pos_267 = mark
              val res_268 = EndOfLine()
              reset(pos_267)
              if (res_268.isSuccess) Failure(ParseFailed("Neglook failed", pos_267))
              else {
                Try(PEmpty)
              }
            }
            catPart_266 <- any.map { char_269 => PLeaf(char_269.toString) }
          } yield PBranch("catPart_256", Seq(catPart_265, catPart_266))
          res_264.recoverWith { case p: ParseError =>
            reset(pos0_263)
            Failure(p)
          }
        }

        var buf_259 = ArrayBuffer.empty[PTree]
        var pos_260 = mark
        var res_261 = subMatch_262
        res_261.recover { _ => reset(pos_260) }
        while (res_261.isSuccess) {
          buf_259 += res_261.get
          pos_260 = mark
          res_261 = subMatch_262
          res_261.recover { _ => reset(pos_260) }
        }
        Try(PBranch("catPart_256", buf_259.toSeq))
      }
      catPart_257 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_255, catPart_256, catPart_257))
    res_254.recoverWith { case p: ParseError =>
      reset(pos0_253)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_270 = mark
    val res_271 = for {
      catPart_272 <- expect('&').map { char_274 => PLeaf(char_274.toString) }
      catPart_273 <- Spacing()
    } yield PBranch("AND", Seq(catPart_272, catPart_273))
    res_271.recoverWith { case p: ParseError =>
      reset(pos0_270)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_275 = mark
    val res_276 = {
      val pos0_278 = mark
      val res_279 = for {
        catPart_280 <- Char()
        catPart_281 <- expect('-').map { char_283 => PLeaf(char_283.toString) }
        catPart_282 <- Char()
      } yield PBranch("Range", Seq(catPart_280, catPart_281, catPart_282))
      res_279.recoverWith { case p: ParseError =>
        reset(pos0_278)
        Failure(p)
      }
    }
    res_276.recoverWith { case err_277: ParseError =>
      reset(pos_275)
      Char().recoverWith { case err_284: ParseError =>
        reset(pos_275)
        Failure(err_277 ~ err_284 ~ ParseFailed("", pos_275))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_285 = mark
    val res_286 = for {
      catPart_287 <- Spacing()
      catPart_288 <- {
        val pos0_290 = mark
        val res_291 = for {
          catPart_292 <- Definition()
          catPart_293 <- {
            def subMatch_297 = Definition()

            var buf_294 = ArrayBuffer.empty[PTree]
            var pos_295 = mark
            var res_296 = subMatch_297
            res_296.recover { _ => reset(pos_295) }
            while (res_296.isSuccess) {
              buf_294 += res_296.get
              pos_295 = mark
              res_296 = subMatch_297
              res_296.recover { _ => reset(pos_295) }
            }
            Try(PBranch("catPart_293", buf_294.toSeq))
          }
        } yield PBranch("catPart_288", Seq(catPart_292, catPart_293))
        res_291.recoverWith { case p: ParseError =>
          reset(pos0_290)
          Failure(p)
        }
      }
      catPart_289 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_287, catPart_288, catPart_289))
    res_286.recoverWith { case p: ParseError =>
      reset(pos0_285)
      Failure(p)
    }
  }
}
