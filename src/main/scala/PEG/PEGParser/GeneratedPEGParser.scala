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
          catPart_101 <- {
            val pos_107 = mark
            any.map { x => PLeaf(x.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_107)
                Failure(p ~ ParseFailed("Expected any char", pos_107))
              }
          }
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
    def subMatch_171 = {
      val pos_172 = mark
      Prefix().recoverWith { case p: ParseError =>
        reset(pos_172)
        Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_172))
      }
    }

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
    val pos0_173 = mark
    val res_174 = for {
      catPart_175 <- Primary()
      catPart_176 <- {
        val pos_177 = mark
        val res_178 = {
          val pos_180 = mark
          QUESTION().recoverWith { case err_181: ParseError =>
            reset(pos_180)
            STAR().recoverWith { case err_182: ParseError =>
              reset(pos_180)
              PLUS().recoverWith { case err_183: ParseError =>
                reset(pos_180)
                Failure(err_181 ~ err_182 ~ err_183 ~ ParseFailed("", pos_180))
              }
            }
          }
        }
        res_178.recoverWith { case err_179: ParseError =>
          reset(pos_177)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_175, catPart_176))
    res_174.recoverWith { case p: ParseError =>
      reset(pos0_173)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_184 = mark
    expect(' ').map { char_185 => PLeaf(char_185.toString) }
      .recoverWith { case err_186: ParseError =>
        reset(pos_184)
        expect('\t').map { char_187 => PLeaf(char_187.toString) }
          .recoverWith { case err_188: ParseError =>
            reset(pos_184)
            EndOfLine().recoverWith { case err_189: ParseError =>
              reset(pos_184)
              Failure(err_186 ~ err_188 ~ err_189 ~ ParseFailed("", pos_184))
            }
          }
      }
  }


  def OPEN(): Try[PTree] = {
    val pos0_190 = mark
    val res_191 = for {
      catPart_192 <- expect('(').map { char_194 => PLeaf(char_194.toString) }
      catPart_193 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_192, catPart_193))
    res_191.recoverWith { case p: ParseError =>
      reset(pos0_190)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_195 = mark
    val res_196 = {
      val pos_197 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_197)
          Failure(p ~ ParseFailed("Expected any char", pos_197))
        }
    }
    reset(pos_195)
    if (res_196.isSuccess) Failure(ParseFailed("Neglook failed", pos_195))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_198 = mark
    val res_199 = {
      val pos0_201 = mark
      val res_202 = for {
        catPart_203 <- Identifier()
        catPart_204 <- {
          val pos_205 = mark
          val res_206 = LEFTARROW()
          reset(pos_205)
          if (res_206.isSuccess) Failure(ParseFailed("Neglook failed", pos_205))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_203, catPart_204))
      res_202.recoverWith { case p: ParseError =>
        reset(pos0_201)
        Failure(p)
      }
    }
    res_199.recoverWith { case err_200: ParseError =>
      reset(pos_198)
      val res_207 = {
        val pos0_209 = mark
        val res_210 = for {
          catPart_211 <- OPEN()
          catPart_212 <- Expression()
          catPart_213 <- CLOSE()
        } yield PBranch("Primary", Seq(catPart_211, catPart_212, catPart_213))
        res_210.recoverWith { case p: ParseError =>
          reset(pos0_209)
          Failure(p)
        }
      }
      res_207.recoverWith { case err_208: ParseError =>
        reset(pos_198)
        Literal().recoverWith { case err_214: ParseError =>
          reset(pos_198)
          Class().recoverWith { case err_215: ParseError =>
            reset(pos_198)
            DOT().recoverWith { case err_216: ParseError =>
              reset(pos_198)
              Failure(err_200 ~ err_208 ~ err_214 ~ err_215 ~ err_216 ~ ParseFailed("", pos_198))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_220 = {
      val pos_221 = mark
      Space().recoverWith { case err_222: ParseError =>
        reset(pos_221)
        Comment().recoverWith { case err_223: ParseError =>
          reset(pos_221)
          Failure(err_222 ~ err_223 ~ ParseFailed("", pos_221))
        }
      }
    }

    var buf_217 = ArrayBuffer.empty[PTree]
    var pos_218 = mark
    var res_219 = subMatch_220
    res_219.recover { _ => reset(pos_218) }
    while (res_219.isSuccess) {
      buf_217 += res_219.get
      pos_218 = mark
      res_219 = subMatch_220
      res_219.recover { _ => reset(pos_218) }
    }
    Try(PBranch("Spacing", buf_217.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_224 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_225 => PLeaf(char_225.toString) }
      .recoverWith { case p: ParseError =>
        reset(pos_224)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_224))
      }
  }


  def Prefix(): Try[PTree] = {
    val pos0_226 = mark
    val res_227 = for {
      catPart_228 <- {
        val pos_230 = mark
        val res_231 = {
          val pos_233 = mark
          AND().recoverWith { case err_234: ParseError =>
            reset(pos_233)
            NOT().recoverWith { case err_235: ParseError =>
              reset(pos_233)
              Failure(err_234 ~ err_235 ~ ParseFailed("", pos_233))
            }
          }
        }
        res_231.recoverWith { case err_232: ParseError =>
          reset(pos_230)
          Try(PEmpty)
        }
      }
      catPart_229 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_228, catPart_229))
    res_227.recoverWith { case p: ParseError =>
      reset(pos0_226)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_236 = mark
    val res_237 = for {
      catPart_238 <- IdentStart()
      catPart_239 <- {
        def subMatch_244 = {
          val pos_245 = mark
          IdentCont().recoverWith { case p: ParseError =>
            reset(pos_245)
            Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_245))
          }
        }

        var buf_241 = ArrayBuffer.empty[PTree]
        var pos_242 = mark
        var res_243 = subMatch_244
        res_243.recover { _ => reset(pos_242) }
        while (res_243.isSuccess) {
          buf_241 += res_243.get
          pos_242 = mark
          res_243 = subMatch_244
          res_243.recover { _ => reset(pos_242) }
        }
        Try(PBranch("catPart_239", buf_241.toSeq))
      }
      catPart_240 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_238, catPart_239, catPart_240))
    res_237.recoverWith { case p: ParseError =>
      reset(pos0_236)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_246 = mark
    val res_247 = for {
      catPart_248 <- expect('/').map { char_250 => PLeaf(char_250.toString) }
      catPart_249 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_248, catPart_249))
    res_247.recoverWith { case p: ParseError =>
      reset(pos0_246)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_251 = mark
    val res_252 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_253 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_254 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_253, catPart_254))
    res_252.recoverWith { case p: ParseError =>
      reset(pos0_251)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_255 = mark
    val res_256 = for {
      catPart_257 <- expect('#').map { char_260 => PLeaf(char_260.toString) }
      catPart_258 <- {
        def subMatch_264 = {
          val pos0_265 = mark
          val res_266 = for {
            catPart_267 <- {
              val pos_269 = mark
              val res_270 = EndOfLine()
              reset(pos_269)
              if (res_270.isSuccess) Failure(ParseFailed("Neglook failed", pos_269))
              else {
                Try(PEmpty)
              }
            }
            catPart_268 <- {
              val pos_271 = mark
              any.map { x => PLeaf(x.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_271)
                  Failure(p ~ ParseFailed("Expected any char", pos_271))
                }
            }
          } yield PBranch("catPart_258", Seq(catPart_267, catPart_268))
          res_266.recoverWith { case p: ParseError =>
            reset(pos0_265)
            Failure(p)
          }
        }

        var buf_261 = ArrayBuffer.empty[PTree]
        var pos_262 = mark
        var res_263 = subMatch_264
        res_263.recover { _ => reset(pos_262) }
        while (res_263.isSuccess) {
          buf_261 += res_263.get
          pos_262 = mark
          res_263 = subMatch_264
          res_263.recover { _ => reset(pos_262) }
        }
        Try(PBranch("catPart_258", buf_261.toSeq))
      }
      catPart_259 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_257, catPart_258, catPart_259))
    res_256.recoverWith { case p: ParseError =>
      reset(pos0_255)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_272 = mark
    val res_273 = for {
      catPart_274 <- expect('&').map { char_276 => PLeaf(char_276.toString) }
      catPart_275 <- Spacing()
    } yield PBranch("AND", Seq(catPart_274, catPart_275))
    res_273.recoverWith { case p: ParseError =>
      reset(pos0_272)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_277 = mark
    val res_278 = {
      val pos0_280 = mark
      val res_281 = for {
        catPart_282 <- Char()
        catPart_283 <- expect('-').map { char_285 => PLeaf(char_285.toString) }
        catPart_284 <- Char()
      } yield PBranch("Range", Seq(catPart_282, catPart_283, catPart_284))
      res_281.recoverWith { case p: ParseError =>
        reset(pos0_280)
        Failure(p)
      }
    }
    res_278.recoverWith { case err_279: ParseError =>
      reset(pos_277)
      Char().recoverWith { case err_286: ParseError =>
        reset(pos_277)
        Failure(err_279 ~ err_286 ~ ParseFailed("", pos_277))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_287 = mark
    val res_288 = for {
      catPart_289 <- Spacing()
      catPart_290 <- {
        val pos0_292 = mark
        val res_293 = for {
          catPart_294 <- Definition()
          catPart_295 <- {
            def subMatch_299 = {
              val pos_300 = mark
              Definition().recoverWith { case p: ParseError =>
                reset(pos_300)
                Failure(p ~ ParseFailed("expected Var 'Definition'", pos_300))
              }
            }

            var buf_296 = ArrayBuffer.empty[PTree]
            var pos_297 = mark
            var res_298 = subMatch_299
            res_298.recover { _ => reset(pos_297) }
            while (res_298.isSuccess) {
              buf_296 += res_298.get
              pos_297 = mark
              res_298 = subMatch_299
              res_298.recover { _ => reset(pos_297) }
            }
            Try(PBranch("catPart_295", buf_296.toSeq))
          }
        } yield PBranch("catPart_290", Seq(catPart_294, catPart_295))
        res_293.recoverWith { case p: ParseError =>
          reset(pos0_292)
          Failure(p)
        }
      }
      catPart_291 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_289, catPart_290, catPart_291))
    res_288.recoverWith { case p: ParseError =>
      reset(pos0_287)
      Failure(p)
    }
  }
}
