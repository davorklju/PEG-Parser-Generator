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
      Failure(p ~ ParseFailed("expected DOT",pos0_1))
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
      expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
        .map { char_142 => PLeaf(char_142.toString) }
        .recoverWith { case err_143: ParseError =>
          reset(pos_140)
          Failure(err_141 ~ err_143 ~ ParseFailed("", pos_140))
        }
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_144 = mark
    val res_145 = for {
      catPart_146 <- expect('!').map { char_148 => PLeaf(char_148.toString) }
      catPart_147 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_146, catPart_147))
    res_145.recoverWith { case p: ParseError =>
      reset(pos0_144)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_149 = mark
    val res_150 = for {
      catPart_151 <- expect('?').map { char_153 => PLeaf(char_153.toString) }
      catPart_152 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_151, catPart_152))
    res_150.recoverWith { case p: ParseError =>
      reset(pos0_149)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_154 = mark
    val res_155 = for {
      catPart_156 <- Sequence()
      catPart_157 <- {
        def subMatch_161 = {
          val pos0_162 = mark
          val res_163 = for {
            catPart_164 <- SLASH()
            catPart_165 <- Sequence()
          } yield PBranch("catPart_157", Seq(catPart_164, catPart_165))
          res_163.recoverWith { case p: ParseError =>
            reset(pos0_162)
            Failure(p)
          }
        }

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
        Try(PBranch("catPart_157", buf_158.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_156, catPart_157))
    res_155.recoverWith { case p: ParseError =>
      reset(pos0_154)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def subMatch_169 = Prefix()

    var buf_166 = ArrayBuffer.empty[PTree]
    var pos_167 = mark
    var res_168 = subMatch_169
    res_168.recover { _ => reset(pos_167) }
    while (res_168.isSuccess) {
      buf_166 += res_168.get
      pos_167 = mark
      res_168 = subMatch_169
      res_168.recover { _ => reset(pos_167) }
    }
    Try(PBranch("Sequence", buf_166.toSeq))
  }


  def Suffix(): Try[PTree] = {
    val pos0_170 = mark
    val res_171 = for {
      catPart_172 <- Primary()
      catPart_173 <- {
        val pos_174 = mark
        val res_175 = {
          val pos_177 = mark
          QUESTION().recoverWith { case err_178: ParseError =>
            reset(pos_177)
            STAR().recoverWith { case err_179: ParseError =>
              reset(pos_177)
              PLUS().recoverWith { case err_180: ParseError =>
                reset(pos_177)
                Failure(err_178 ~ err_179 ~ err_180 ~ ParseFailed("", pos_177))
              }
            }
          }
        }
        res_175.recoverWith { case err_176: ParseError =>
          reset(pos_174)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_172, catPart_173))
    res_171.recoverWith { case p: ParseError =>
      reset(pos0_170)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_181 = mark
    expect(' ').map { char_182 => PLeaf(char_182.toString) }
      .recoverWith { case err_183: ParseError =>
        reset(pos_181)
        expect('\t').map { char_184 => PLeaf(char_184.toString) }
          .recoverWith { case err_185: ParseError =>
            reset(pos_181)
            EndOfLine().recoverWith { case err_186: ParseError =>
              reset(pos_181)
              Failure(err_183 ~ err_185 ~ err_186 ~ ParseFailed("", pos_181))
            }
          }
      }
  }


  def OPEN(): Try[PTree] = {
    val pos0_187 = mark
    val res_188 = for {
      catPart_189 <- expect('(').map { char_191 => PLeaf(char_191.toString) }
      catPart_190 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_189, catPart_190))
    res_188.recoverWith { case p: ParseError =>
      reset(pos0_187)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_192 = mark
    val res_193 = {
      val pos_194 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_194)
          Failure(p ~ ParseFailed("Expected any char", pos_194))
        }
    }
    reset(pos_192)
    if (res_193.isSuccess) Failure(ParseFailed("Neglook failed", pos_192))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_195 = mark
    val res_196 = {
      val pos0_198 = mark
      val res_199 = for {
        catPart_200 <- Identifier()
        catPart_201 <- {
          val pos_202 = mark
          val res_203 = LEFTARROW()
          reset(pos_202)
          if (res_203.isSuccess) Failure(ParseFailed("Neglook failed", pos_202))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_200, catPart_201))
      res_199.recoverWith { case p: ParseError =>
        reset(pos0_198)
        Failure(p)
      }
    }
    res_196.recoverWith { case err_197: ParseError =>
      reset(pos_195)
      val res_204 = {
        val pos0_206 = mark
        val res_207 = for {
          catPart_208 <- OPEN()
          catPart_209 <- Expression()
          catPart_210 <- CLOSE()
        } yield PBranch("Primary", Seq(catPart_208, catPart_209, catPart_210))
        res_207.recoverWith { case p: ParseError =>
          reset(pos0_206)
          Failure(p)
        }
      }
      res_204.recoverWith { case err_205: ParseError =>
        reset(pos_195)
        Literal().recoverWith { case err_211: ParseError =>
          reset(pos_195)
          Class().recoverWith { case err_212: ParseError =>
            reset(pos_195)
            DOT().recoverWith { case err_213: ParseError =>
              reset(pos_195)
              Failure(err_197 ~ err_205 ~ err_211 ~ err_212 ~ err_213 ~ ParseFailed("", pos_195))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_217 = {
      val pos_218 = mark
      Space().recoverWith { case err_219: ParseError =>
        reset(pos_218)
        Comment().recoverWith { case err_220: ParseError =>
          reset(pos_218)
          Failure(err_219 ~ err_220 ~ ParseFailed("", pos_218))
        }
      }
    }

    var buf_214 = ArrayBuffer.empty[PTree]
    var pos_215 = mark
    var res_216 = subMatch_217
    res_216.recover { _ => reset(pos_215) }
    while (res_216.isSuccess) {
      buf_214 += res_216.get
      pos_215 = mark
      res_216 = subMatch_217
      res_216.recover { _ => reset(pos_215) }
    }
    Try(PBranch("Spacing", buf_214.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_221 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_222 => PLeaf(char_222.toString) }
      .recoverWith { case p: ParseError =>
        reset(pos_221)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_221))
      }
  }


  def Prefix(): Try[PTree] = {
    val pos0_223 = mark
    val res_224 = for {
      catPart_225 <- {
        val pos_227 = mark
        val res_228 = {
          val pos_230 = mark
          AND().recoverWith { case err_231: ParseError =>
            reset(pos_230)
            NOT().recoverWith { case err_232: ParseError =>
              reset(pos_230)
              Failure(err_231 ~ err_232 ~ ParseFailed("", pos_230))
            }
          }
        }
        res_228.recoverWith { case err_229: ParseError =>
          reset(pos_227)
          Try(PEmpty)
        }
      }
      catPart_226 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_225, catPart_226))
    res_224.recoverWith { case p: ParseError =>
      reset(pos0_223)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_233 = mark
    val res_234 = for {
      catPart_235 <- IdentStart()
      catPart_236 <- {
        def subMatch_241 = IdentCont()

        var buf_238 = ArrayBuffer.empty[PTree]
        var pos_239 = mark
        var res_240 = subMatch_241
        res_240.recover { _ => reset(pos_239) }
        while (res_240.isSuccess) {
          buf_238 += res_240.get
          pos_239 = mark
          res_240 = subMatch_241
          res_240.recover { _ => reset(pos_239) }
        }
        Try(PBranch("catPart_236", buf_238.toSeq))
      }
      catPart_237 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_235, catPart_236, catPart_237))
    res_234.recoverWith { case p: ParseError =>
      reset(pos0_233)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_242 = mark
    val res_243 = for {
      catPart_244 <- expect('/').map { char_246 => PLeaf(char_246.toString) }
      catPart_245 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_244, catPart_245))
    res_243.recoverWith { case p: ParseError =>
      reset(pos0_242)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_247 = mark
    val res_248 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_249 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_250 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_249, catPart_250))
    res_248.recoverWith { case p: ParseError =>
      reset(pos0_247)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_251 = mark
    val res_252 = for {
      catPart_253 <- expect('#').map { char_256 => PLeaf(char_256.toString) }
      catPart_254 <- {
        def subMatch_260 = {
          val pos0_261 = mark
          val res_262 = for {
            catPart_263 <- {
              val pos_265 = mark
              val res_266 = EndOfLine()
              reset(pos_265)
              if (res_266.isSuccess) Failure(ParseFailed("Neglook failed", pos_265))
              else {
                Try(PEmpty)
              }
            }
            catPart_264 <- any.map { char_267 => PLeaf(char_267.toString) }
          } yield PBranch("catPart_254", Seq(catPart_263, catPart_264))
          res_262.recoverWith { case p: ParseError =>
            reset(pos0_261)
            Failure(p)
          }
        }

        var buf_257 = ArrayBuffer.empty[PTree]
        var pos_258 = mark
        var res_259 = subMatch_260
        res_259.recover { _ => reset(pos_258) }
        while (res_259.isSuccess) {
          buf_257 += res_259.get
          pos_258 = mark
          res_259 = subMatch_260
          res_259.recover { _ => reset(pos_258) }
        }
        Try(PBranch("catPart_254", buf_257.toSeq))
      }
      catPart_255 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_253, catPart_254, catPart_255))
    res_252.recoverWith { case p: ParseError =>
      reset(pos0_251)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_268 = mark
    val res_269 = for {
      catPart_270 <- expect('&').map { char_272 => PLeaf(char_272.toString) }
      catPart_271 <- Spacing()
    } yield PBranch("AND", Seq(catPart_270, catPart_271))
    res_269.recoverWith { case p: ParseError =>
      reset(pos0_268)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_273 = mark
    val res_274 = {
      val pos0_276 = mark
      val res_277 = for {
        catPart_278 <- Char()
        catPart_279 <- expect('-').map { char_281 => PLeaf(char_281.toString) }
        catPart_280 <- Char()
      } yield PBranch("Range", Seq(catPart_278, catPart_279, catPart_280))
      res_277.recoverWith { case p: ParseError =>
        reset(pos0_276)
        Failure(p)
      }
    }
    res_274.recoverWith { case err_275: ParseError =>
      reset(pos_273)
      Char().recoverWith { case err_282: ParseError =>
        reset(pos_273)
        Failure(err_275 ~ err_282 ~ ParseFailed("", pos_273))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_283 = mark
    val res_284 = for {
      catPart_285 <- Spacing()
      catPart_286 <- {
        val pos0_288 = mark
        val res_289 = for {
          catPart_290 <- Definition()
          catPart_291 <- {
            def subMatch_295 = Definition()

            var buf_292 = ArrayBuffer.empty[PTree]
            var pos_293 = mark
            var res_294 = subMatch_295
            res_294.recover { _ => reset(pos_293) }
            while (res_294.isSuccess) {
              buf_292 += res_294.get
              pos_293 = mark
              res_294 = subMatch_295
              res_294.recover { _ => reset(pos_293) }
            }
            Try(PBranch("catPart_291", buf_292.toSeq))
          }
        } yield PBranch("catPart_286", Seq(catPart_290, catPart_291))
        res_289.recoverWith { case p: ParseError =>
          reset(pos0_288)
          Failure(p)
        }
      }
      catPart_287 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_285, catPart_286, catPart_287))
    res_284.recoverWith { case p: ParseError =>
      reset(pos0_283)
      Failure(p)
    }
  }
}
