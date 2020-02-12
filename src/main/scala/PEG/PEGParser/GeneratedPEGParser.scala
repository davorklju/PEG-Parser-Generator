package PEG.PEGParser

import PEG.lexparse.{Lexer, Parser}
import PEG.data.implicits._
import PEG.data._
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
            def catPart_9_sub_13 = Definition()

            var buf_10 = ArrayBuffer.empty[PTree]
            var pos_11 = mark
            var res_12 = catPart_9_sub_13
            res_12.recover { _ => reset(pos_11) }
            while (res_12.isSuccess) {
              buf_10 += res_12.get
              pos_11 = mark
              res_12 = catPart_9_sub_13
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
        def catPart_25_sub_29 = {
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
        var res_28 = catPart_25_sub_29
        res_28.recover { _ => reset(pos_27) }
        while (res_28.isSuccess) {
          buf_26 += res_28.get
          pos_27 = mark
          res_28 = catPart_25_sub_29
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


  def Expression(): Try[PTree] = {
    val pos0_40 = mark
    val res_41 = for {
      catPart_42 <- Sequence()
      catPart_43 <- {
        def catPart_43_sub_47 = {
          val pos0_48 = mark
          val res_49 = for {
            catPart_50 <- SLASH()
            catPart_51 <- Sequence()
          } yield PBranch("catPart_43", Seq(catPart_50, catPart_51))
          res_49.recoverWith { case p: ParseError[Char] =>
            reset(pos0_48)
            Failure(p)
          }
        }

        var buf_44 = ArrayBuffer.empty[PTree]
        var pos_45 = mark
        var res_46 = catPart_43_sub_47
        res_46.recover { _ => reset(pos_45) }
        while (res_46.isSuccess) {
          buf_44 += res_46.get
          pos_45 = mark
          res_46 = catPart_43_sub_47
          res_46.recover { _ => reset(pos_45) }
        }
        Try(PBranch("catPart_43", buf_44.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_42, catPart_43))
    res_41.recoverWith { case p: ParseError[Char] =>
      reset(pos0_40)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def Sequence_sub_55 = Prefix()

    var buf_52 = ArrayBuffer.empty[PTree]
    var pos_53 = mark
    var res_54 = Sequence_sub_55
    res_54.recover { _ => reset(pos_53) }
    while (res_54.isSuccess) {
      buf_52 += res_54.get
      pos_53 = mark
      res_54 = Sequence_sub_55
      res_54.recover { _ => reset(pos_53) }
    }
    Try(PBranch("Sequence", buf_52.toSeq))
  }


  def Prefix(): Try[PTree] = {
    val pos0_56 = mark
    val res_57 = for {
      catPart_58 <- {
        val pos_60 = mark
        val res_61 = {
          val pos_63 = mark
          AND().recoverWith { case err_64: ParseError[Char] =>
            reset(pos_63)
            NOT().recoverWith { case err_65: ParseError[Char] =>
              reset(pos_63)
              Failure(err_64 ~ err_65 ~ ParseFailed("", pos_63))
            }
          }
        }
        res_61.recoverWith { case err_62: ParseError[Char] =>
          reset(pos_60)
          Try(PEmpty)
        }
      }
      catPart_59 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_58, catPart_59))
    res_57.recoverWith { case p: ParseError[Char] =>
      reset(pos0_56)
      Failure(p)
    }
  }


  def Suffix(): Try[PTree] = {
    val pos0_66 = mark
    val res_67 = for {
      catPart_68 <- Primary()
      catPart_69 <- {
        val pos_70 = mark
        val res_71 = {
          val pos_73 = mark
          QUESTION().recoverWith { case err_74: ParseError[Char] =>
            reset(pos_73)
            STAR().recoverWith { case err_75: ParseError[Char] =>
              reset(pos_73)
              PLUS().recoverWith { case err_76: ParseError[Char] =>
                reset(pos_73)
                Failure(err_74 ~ err_75 ~ err_76 ~ ParseFailed("", pos_73))
              }
            }
          }
        }
        res_71.recoverWith { case err_72: ParseError[Char] =>
          reset(pos_70)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_68, catPart_69))
    res_67.recoverWith { case p: ParseError[Char] =>
      reset(pos0_66)
      Failure(p)
    }
  }


  val cache_78 = mutable.HashMap.empty[Int, (Try[PTree], Int)]

  def Primary(): Try[PTree] = {
    def parser_77(): Try[PTree] = {
      val pos_82 = mark
      val res_83 = {
        val pos0_85 = mark
        val res_86 = for {
          catPart_87 <- Identifier()
          catPart_88 <- {
            val pos_89 = mark
            val res_90 = {
              val pos0_91 = mark
              val res_92 = for {
                catPart_93 <- {
                  val pos_95 = mark
                  STAR().recoverWith { case err_96: ParseError[Char] =>
                    reset(pos_95)
                    Try(PEmpty)
                  }
                }
                catPart_94 <- LEFTARROW()
              } yield PBranch("catPart_88", Seq(catPart_93, catPart_94))
              res_92.recoverWith { case p: ParseError[Char] =>
                reset(pos0_91)
                Failure(p)
              }
            }
            reset(pos_89)
            if (res_90.isSuccess) Failure(ParseFailed("Neglook failed", pos_89))
            else {
              Try(PEmpty)
            }
          }
        } yield PBranch("Primary", Seq(catPart_87, catPart_88))
        res_86.recoverWith { case p: ParseError[Char] =>
          reset(pos0_85)
          Failure(p)
        }
      }
      res_83.recoverWith { case err_84: ParseError[Char] =>
        reset(pos_82)
        val res_97 = {
          val pos0_99 = mark
          val res_100 = for {
            catPart_101 <- OPEN()
            catPart_102 <- Expression()
            catPart_103 <- CLOSE()
          } yield PBranch("Primary", Seq(catPart_101, catPart_102, catPart_103))
          res_100.recoverWith { case p: ParseError[Char] =>
            reset(pos0_99)
            Failure(p)
          }
        }
        res_97.recoverWith { case err_98: ParseError[Char] =>
          reset(pos_82)
          Literal().recoverWith { case err_104: ParseError[Char] =>
            reset(pos_82)
            Class().recoverWith { case err_105: ParseError[Char] =>
              reset(pos_82)
              DOT().recoverWith { case err_106: ParseError[Char] =>
                reset(pos_82)
                Failure(err_84 ~ err_98 ~ err_104 ~ err_105 ~ err_106 ~ ParseFailed("", pos_82))
              }
            }
          }
        }
      }
    }

    if (!cache_78.contains(mark)) {
      val init_80 = mark
      cache_78(init_80) = parser_77() -> mark
      reset(init_80)
    }
    val (res_79, pos_81) = cache_78(mark)
    reset(pos_81)
    res_79
  }


  def Identifier(): Try[PTree] = {
    val pos0_107 = mark
    val res_108 = for {
      catPart_109 <- IdentStart()
      catPart_110 <- {
        def catPart_110_sub_115 = IdentCont()

        var buf_112 = ArrayBuffer.empty[PTree]
        var pos_113 = mark
        var res_114 = catPart_110_sub_115
        res_114.recover { _ => reset(pos_113) }
        while (res_114.isSuccess) {
          buf_112 += res_114.get
          pos_113 = mark
          res_114 = catPart_110_sub_115
          res_114.recover { _ => reset(pos_113) }
        }
        Try(PBranch("catPart_110", buf_112.toSeq))
      }
      catPart_111 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_109, catPart_110, catPart_111))
    res_108.recoverWith { case p: ParseError[Char] =>
      reset(pos0_107)
      Failure(p)
    }
  }


  def IdentStart(): Try[PTree] = {
    val pos_116 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_117 => PLeaf(char_117.toString) }
      .recoverWith { case p: ParseError[Char] =>
        reset(pos_116)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_116))
      }
  }


  def IdentCont(): Try[PTree] = {
    val pos_118 = mark
    IdentStart().recoverWith { case err_119: ParseError[Char] =>
      reset(pos_118)
      expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
        .map { char_120 => PLeaf(char_120.toString) }
        .recoverWith { case err_121: ParseError[Char] =>
          reset(pos_118)
          Failure(err_119 ~ err_121 ~ ParseFailed("", pos_118))
        }
    }
  }


  def Action(): Try[PTree] = {
    val pos0_122 = mark
    val res_123 = for {
      catPart_124 <- OPENCURLY()
      catPart_125 <- {
        val pos0_131 = mark
        val res_132 = for {
          catPart_133 <- {
            val pos0_135 = mark
            val res_136 = for {
              catPart_137 <- {
                val pos_139 = mark
                val res_140 = expect('|', '}')
                reset(pos_139)
                if (res_140.isSuccess) Failure(ParseFailed("Neglook failed", pos_139))
                else {
                  Try(PEmpty)
                }
              }
              catPart_138 <- any.map { char_141 => PLeaf(char_141.toString) }
            } yield PBranch("catPart_133", Seq(catPart_137, catPart_138))
            res_136.recoverWith { case p: ParseError[Char] =>
              reset(pos0_135)
              Failure(p)
            }
          }
          catPart_134 <- {
            def catPart_134_sub_145 = {
              val pos0_146 = mark
              val res_147 = for {
                catPart_148 <- {
                  val pos_150 = mark
                  val res_151 = expect('|', '}')
                  reset(pos_150)
                  if (res_151.isSuccess) Failure(ParseFailed("Neglook failed", pos_150))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_149 <- any.map { char_152 => PLeaf(char_152.toString) }
              } yield PBranch("catPart_134", Seq(catPart_148, catPart_149))
              res_147.recoverWith { case p: ParseError[Char] =>
                reset(pos0_146)
                Failure(p)
              }
            }

            var buf_142 = ArrayBuffer.empty[PTree]
            var pos_143 = mark
            var res_144 = catPart_134_sub_145
            res_144.recover { _ => reset(pos_143) }
            while (res_144.isSuccess) {
              buf_142 += res_144.get
              pos_143 = mark
              res_144 = catPart_134_sub_145
              res_144.recover { _ => reset(pos_143) }
            }
            Try(PBranch("catPart_134", buf_142.toSeq))
          }
        } yield PBranch("catPart_125", Seq(catPart_133, catPart_134))
        res_132.recoverWith { case p: ParseError[Char] =>
          reset(pos0_131)
          Failure(p)
        }
      }
      catPart_126 <- VERTBAR()
      catPart_127 <- {
        def catPart_127_sub_156 = ActionIdent()

        var buf_153 = ArrayBuffer.empty[PTree]
        var pos_154 = mark
        var res_155 = catPart_127_sub_156
        res_155.recover { _ => reset(pos_154) }
        while (res_155.isSuccess) {
          buf_153 += res_155.get
          pos_154 = mark
          res_155 = catPart_127_sub_156
          res_155.recover { _ => reset(pos_154) }
        }
        Try(PBranch("catPart_127", buf_153.toSeq))
      }
      catPart_128 <- VERTBAR()
      catPart_129 <- {
        def catPart_129_sub_160 = {
          val pos0_161 = mark
          val res_162 = for {
            catPart_163 <- {
              val pos_165 = mark
              val res_166 = expect('}')
              reset(pos_165)
              if (res_166.isSuccess) Failure(ParseFailed("Neglook failed", pos_165))
              else {
                Try(PEmpty)
              }
            }
            catPart_164 <- any.map { char_167 => PLeaf(char_167.toString) }
          } yield PBranch("catPart_129", Seq(catPart_163, catPart_164))
          res_162.recoverWith { case p: ParseError[Char] =>
            reset(pos0_161)
            Failure(p)
          }
        }

        var buf_157 = ArrayBuffer.empty[PTree]
        var pos_158 = mark
        var res_159 = catPart_129_sub_160
        res_159.recover { _ => reset(pos_158) }
        while (res_159.isSuccess) {
          buf_157 += res_159.get
          pos_158 = mark
          res_159 = catPart_129_sub_160
          res_159.recover { _ => reset(pos_158) }
        }
        Try(PBranch("catPart_129", buf_157.toSeq))
      }
      catPart_130 <- CLOSECURLY()
    } yield PBranch("Action", Seq(catPart_124, catPart_125, catPart_126, catPart_127, catPart_128, catPart_129, catPart_130))
    res_123.recoverWith { case p: ParseError[Char] =>
      reset(pos0_122)
      Failure(p)
    }
  }


  def ActionIdent(): Try[PTree] = {
    val pos_168 = mark
    val res_169 = {
      val pos0_171 = mark
      val res_172 = for {
        catPart_173 <- Identifier()
        catPart_174 <- OPEN()
        catPart_175 <- ActionIdent()
        catPart_176 <- {
          def catPart_176_sub_181 = {
            val pos0_182 = mark
            val res_183 = for {
              catPart_184 <- COMMA()
              catPart_185 <- ActionIdent()
            } yield PBranch("catPart_176", Seq(catPart_184, catPart_185))
            res_183.recoverWith { case p: ParseError[Char] =>
              reset(pos0_182)
              Failure(p)
            }
          }

          var buf_178 = ArrayBuffer.empty[PTree]
          var pos_179 = mark
          var res_180 = catPart_176_sub_181
          res_180.recover { _ => reset(pos_179) }
          while (res_180.isSuccess) {
            buf_178 += res_180.get
            pos_179 = mark
            res_180 = catPart_176_sub_181
            res_180.recover { _ => reset(pos_179) }
          }
          Try(PBranch("catPart_176", buf_178.toSeq))
        }
        catPart_177 <- CLOSE()
      } yield PBranch("ActionIdent", Seq(catPart_173, catPart_174, catPart_175, catPart_176, catPart_177))
      res_172.recoverWith { case p: ParseError[Char] =>
        reset(pos0_171)
        Failure(p)
      }
    }
    res_169.recoverWith { case err_170: ParseError[Char] =>
      reset(pos_168)
      Identifier().recoverWith { case err_186: ParseError[Char] =>
        reset(pos_168)
        Failure(err_170 ~ err_186 ~ ParseFailed("", pos_168))
      }
    }
  }


  def Literal(): Try[PTree] = {
    val pos_187 = mark
    val res_188 = {
      val pos0_190 = mark
      val res_191 = for {
        catPart_192 <- expect('\'').map { char_196 => PLeaf(char_196.toString) }
        catPart_193 <- {
          def catPart_193_sub_200 = {
            val pos0_201 = mark
            val res_202 = for {
              catPart_203 <- {
                val pos_205 = mark
                val res_206 = expect('\'')
                reset(pos_205)
                if (res_206.isSuccess) Failure(ParseFailed("Neglook failed", pos_205))
                else {
                  Try(PEmpty)
                }
              }
              catPart_204 <- Char()
            } yield PBranch("catPart_193", Seq(catPart_203, catPart_204))
            res_202.recoverWith { case p: ParseError[Char] =>
              reset(pos0_201)
              Failure(p)
            }
          }

          var buf_197 = ArrayBuffer.empty[PTree]
          var pos_198 = mark
          var res_199 = catPart_193_sub_200
          res_199.recover { _ => reset(pos_198) }
          while (res_199.isSuccess) {
            buf_197 += res_199.get
            pos_198 = mark
            res_199 = catPart_193_sub_200
            res_199.recover { _ => reset(pos_198) }
          }
          Try(PBranch("catPart_193", buf_197.toSeq))
        }
        catPart_194 <- expect('\'').map { char_207 => PLeaf(char_207.toString) }
        catPart_195 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_192, catPart_193, catPart_194, catPart_195))
      res_191.recoverWith { case p: ParseError[Char] =>
        reset(pos0_190)
        Failure(p)
      }
    }
    res_188.recoverWith { case err_189: ParseError[Char] =>
      reset(pos_187)
      val res_208 = {
        val pos0_210 = mark
        val res_211 = for {
          catPart_212 <- expect('"').map { char_216 => PLeaf(char_216.toString) }
          catPart_213 <- {
            def catPart_213_sub_220 = {
              val pos0_221 = mark
              val res_222 = for {
                catPart_223 <- {
                  val pos_225 = mark
                  val res_226 = expect('"')
                  reset(pos_225)
                  if (res_226.isSuccess) Failure(ParseFailed("Neglook failed", pos_225))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_224 <- Char()
              } yield PBranch("catPart_213", Seq(catPart_223, catPart_224))
              res_222.recoverWith { case p: ParseError[Char] =>
                reset(pos0_221)
                Failure(p)
              }
            }

            var buf_217 = ArrayBuffer.empty[PTree]
            var pos_218 = mark
            var res_219 = catPart_213_sub_220
            res_219.recover { _ => reset(pos_218) }
            while (res_219.isSuccess) {
              buf_217 += res_219.get
              pos_218 = mark
              res_219 = catPart_213_sub_220
              res_219.recover { _ => reset(pos_218) }
            }
            Try(PBranch("catPart_213", buf_217.toSeq))
          }
          catPart_214 <- expect('"').map { char_227 => PLeaf(char_227.toString) }
          catPart_215 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_212, catPart_213, catPart_214, catPart_215))
        res_211.recoverWith { case p: ParseError[Char] =>
          reset(pos0_210)
          Failure(p)
        }
      }
      res_208.recoverWith { case err_209: ParseError[Char] =>
        reset(pos_187)
        val res_228 = {
          val pos0_230 = mark
          val res_231 = for {
            catPart_232 <- expect('`').map { char_236 => PLeaf(char_236.toString) }
            catPart_233 <- {
              def catPart_233_sub_240 = {
                val pos0_241 = mark
                val res_242 = for {
                  catPart_243 <- {
                    val pos_245 = mark
                    val res_246 = expect('`')
                    reset(pos_245)
                    if (res_246.isSuccess) Failure(ParseFailed("Neglook failed", pos_245))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_244 <- Char()
                } yield PBranch("catPart_233", Seq(catPart_243, catPart_244))
                res_242.recoverWith { case p: ParseError[Char] =>
                  reset(pos0_241)
                  Failure(p)
                }
              }

              var buf_237 = ArrayBuffer.empty[PTree]
              var pos_238 = mark
              var res_239 = catPart_233_sub_240
              res_239.recover { _ => reset(pos_238) }
              while (res_239.isSuccess) {
                buf_237 += res_239.get
                pos_238 = mark
                res_239 = catPart_233_sub_240
                res_239.recover { _ => reset(pos_238) }
              }
              Try(PBranch("catPart_233", buf_237.toSeq))
            }
            catPart_234 <- expect('`').map { char_247 => PLeaf(char_247.toString) }
            catPart_235 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_232, catPart_233, catPart_234, catPart_235))
          res_231.recoverWith { case p: ParseError[Char] =>
            reset(pos0_230)
            Failure(p)
          }
        }
        res_228.recoverWith { case err_229: ParseError[Char] =>
          reset(pos_187)
          Failure(err_189 ~ err_209 ~ err_229 ~ ParseFailed("", pos_187))
        }
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_248 = mark
    val res_249 = for {
      catPart_250 <- expect('[').map { char_254 => PLeaf(char_254.toString) }
      catPart_251 <- {
        def catPart_251_sub_258 = {
          val pos0_259 = mark
          val res_260 = for {
            catPart_261 <- {
              val pos_263 = mark
              val res_264 = {
                val pos_265 = mark
                val res_266 = for {
                  char_part_267 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_267.toString)))
                res_266.recoverWith { case p: ParseError[Char] =>
                  reset(pos_265)
                  Failure(p ~ ParseFailed("expected ']'", pos_265))
                }
              }
              reset(pos_263)
              if (res_264.isSuccess) Failure(ParseFailed("Neglook failed", pos_263))
              else {
                Try(PEmpty)
              }
            }
            catPart_262 <- Range()
          } yield PBranch("catPart_251", Seq(catPart_261, catPart_262))
          res_260.recoverWith { case p: ParseError[Char] =>
            reset(pos0_259)
            Failure(p)
          }
        }

        var buf_255 = ArrayBuffer.empty[PTree]
        var pos_256 = mark
        var res_257 = catPart_251_sub_258
        res_257.recover { _ => reset(pos_256) }
        while (res_257.isSuccess) {
          buf_255 += res_257.get
          pos_256 = mark
          res_257 = catPart_251_sub_258
          res_257.recover { _ => reset(pos_256) }
        }
        Try(PBranch("catPart_251", buf_255.toSeq))
      }
      catPart_252 <- expect(']').map { char_268 => PLeaf(char_268.toString) }
      catPart_253 <- Spacing()
    } yield PBranch("Class", Seq(catPart_250, catPart_251, catPart_252, catPart_253))
    res_249.recoverWith { case p: ParseError[Char] =>
      reset(pos0_248)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_269 = mark
    val res_270 = {
      val pos0_272 = mark
      val res_273 = for {
        catPart_274 <- Char()
        catPart_275 <- expect('-').map { char_277 => PLeaf(char_277.toString) }
        catPart_276 <- Char()
      } yield PBranch("Range", Seq(catPart_274, catPart_275, catPart_276))
      res_273.recoverWith { case p: ParseError[Char] =>
        reset(pos0_272)
        Failure(p)
      }
    }
    res_270.recoverWith { case err_271: ParseError[Char] =>
      reset(pos_269)
      Char().recoverWith { case err_278: ParseError[Char] =>
        reset(pos_269)
        Failure(err_271 ~ err_278 ~ ParseFailed("", pos_269))
      }
    }
  }


  def Char(): Try[PTree] = {
    val pos_279 = mark
    val res_280 = {
      val pos0_282 = mark
      val res_283 = for {
        catPart_284 <- expect('\\').map { char_286 => PLeaf(char_286.toString) }
        catPart_285 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"').map { char_287 => PLeaf(char_287.toString) }
      } yield PBranch("Char", Seq(catPart_284, catPart_285))
      res_283.recoverWith { case p: ParseError[Char] =>
        reset(pos0_282)
        Failure(p)
      }
    }
    res_280.recoverWith { case err_281: ParseError[Char] =>
      reset(pos_279)
      val res_288 = {
        val pos0_290 = mark
        val res_291 = for {
          catPart_292 <- {
            val pos_294 = mark
            val res_295 = {
              val pos_296 = mark
              val res_297 = for {
                char_part_298 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_298.toString)))
              res_297.recoverWith { case p: ParseError[Char] =>
                reset(pos_296)
                Failure(p ~ ParseFailed("expected '\\'", pos_296))
              }
            }
            reset(pos_294)
            if (res_295.isSuccess) Failure(ParseFailed("Neglook failed", pos_294))
            else {
              Try(PEmpty)
            }
          }
          catPart_293 <- any.map { char_299 => PLeaf(char_299.toString) }
        } yield PBranch("Char", Seq(catPart_292, catPart_293))
        res_291.recoverWith { case p: ParseError[Char] =>
          reset(pos0_290)
          Failure(p)
        }
      }
      res_288.recoverWith { case err_289: ParseError[Char] =>
        reset(pos_279)
        Failure(err_281 ~ err_289 ~ ParseFailed("", pos_279))
      }
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_300 = mark
    val res_301 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_302 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_303 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_302, catPart_303))
    res_301.recoverWith { case p: ParseError[Char] =>
      reset(pos0_300)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_304 = mark
    val res_305 = for {
      catPart_306 <- expect('/').map { char_308 => PLeaf(char_308.toString) }
      catPart_307 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_306, catPart_307))
    res_305.recoverWith { case p: ParseError[Char] =>
      reset(pos0_304)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_309 = mark
    val res_310 = for {
      catPart_311 <- expect('&').map { char_313 => PLeaf(char_313.toString) }
      catPart_312 <- Spacing()
    } yield PBranch("AND", Seq(catPart_311, catPart_312))
    res_310.recoverWith { case p: ParseError[Char] =>
      reset(pos0_309)
      Failure(p)
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_314 = mark
    val res_315 = for {
      catPart_316 <- expect('!').map { char_318 => PLeaf(char_318.toString) }
      catPart_317 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_316, catPart_317))
    res_315.recoverWith { case p: ParseError[Char] =>
      reset(pos0_314)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_319 = mark
    val res_320 = for {
      catPart_321 <- expect('?').map { char_323 => PLeaf(char_323.toString) }
      catPart_322 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_321, catPart_322))
    res_320.recoverWith { case p: ParseError[Char] =>
      reset(pos0_319)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_324 = mark
    val res_325 = for {
      catPart_326 <- expect('*').map { char_328 => PLeaf(char_328.toString) }
      catPart_327 <- Spacing()
    } yield PBranch("STAR", Seq(catPart_326, catPart_327))
    res_325.recoverWith { case p: ParseError[Char] =>
      reset(pos0_324)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_329 = mark
    val res_330 = for {
      catPart_331 <- expect('+').map { char_333 => PLeaf(char_333.toString) }
      catPart_332 <- Spacing()
    } yield PBranch("PLUS", Seq(catPart_331, catPart_332))
    res_330.recoverWith { case p: ParseError[Char] =>
      reset(pos0_329)
      Failure(p)
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_334 = mark
    val res_335 = for {
      catPart_336 <- expect('(').map { char_338 => PLeaf(char_338.toString) }
      catPart_337 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_336, catPart_337))
    res_335.recoverWith { case p: ParseError[Char] =>
      reset(pos0_334)
      Failure(p)
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_339 = mark
    val res_340 = for {
      catPart_341 <- expect(')').map { char_343 => PLeaf(char_343.toString) }
      catPart_342 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_341, catPart_342))
    res_340.recoverWith { case p: ParseError[Char] =>
      reset(pos0_339)
      Failure(p)
    }
  }


  def DOT(): Try[PTree] = {
    val pos0_344 = mark
    val res_345 = for {
      catPart_346 <- expect('.').map { char_348 => PLeaf(char_348.toString) }
      catPart_347 <- Spacing()
    } yield PBranch("DOT", Seq(catPart_346, catPart_347))
    res_345.recoverWith { case p: ParseError[Char] =>
      reset(pos0_344)
      Failure(p)
    }
  }


  def OPENCURLY(): Try[PTree] = {
    val pos0_349 = mark
    val res_350 = for {
      catPart_351 <- expect('{').map { char_353 => PLeaf(char_353.toString) }
      catPart_352 <- Spacing()
    } yield PBranch("OPENCURLY", Seq(catPart_351, catPart_352))
    res_350.recoverWith { case p: ParseError[Char] =>
      reset(pos0_349)
      Failure(p)
    }
  }


  def CLOSECURLY(): Try[PTree] = {
    val pos0_354 = mark
    val res_355 = for {
      catPart_356 <- expect('}').map { char_358 => PLeaf(char_358.toString) }
      catPart_357 <- Spacing()
    } yield PBranch("CLOSECURLY", Seq(catPart_356, catPart_357))
    res_355.recoverWith { case p: ParseError[Char] =>
      reset(pos0_354)
      Failure(p)
    }
  }


  def VERTBAR(): Try[PTree] = {
    val pos0_359 = mark
    val res_360 = for {
      catPart_361 <- expect('|').map { char_363 => PLeaf(char_363.toString) }
      catPart_362 <- Spacing()
    } yield PBranch("VERTBAR", Seq(catPart_361, catPart_362))
    res_360.recoverWith { case p: ParseError[Char] =>
      reset(pos0_359)
      Failure(p)
    }
  }


  def COMMA(): Try[PTree] = {
    val pos0_364 = mark
    val res_365 = for {
      catPart_366 <- expect(',').map { char_368 => PLeaf(char_368.toString) }
      catPart_367 <- Spacing()
    } yield PBranch("COMMA", Seq(catPart_366, catPart_367))
    res_365.recoverWith { case p: ParseError[Char] =>
      reset(pos0_364)
      Failure(p)
    }
  }


  def Spacing(): Try[PTree] = {
    def Spacing_sub_372 = {
      val pos_373 = mark
      Space().recoverWith { case err_374: ParseError[Char] =>
        reset(pos_373)
        Comment().recoverWith { case err_375: ParseError[Char] =>
          reset(pos_373)
          Failure(err_374 ~ err_375 ~ ParseFailed("", pos_373))
        }
      }
    }

    var buf_369 = ArrayBuffer.empty[PTree]
    var pos_370 = mark
    var res_371 = Spacing_sub_372
    res_371.recover { _ => reset(pos_370) }
    while (res_371.isSuccess) {
      buf_369 += res_371.get
      pos_370 = mark
      res_371 = Spacing_sub_372
      res_371.recover { _ => reset(pos_370) }
    }
    Try(PBranch("Spacing", buf_369.toSeq))
  }


  def Comment(): Try[PTree] = {
    val pos0_376 = mark
    val res_377 = for {
      catPart_378 <- expect('#').map { char_381 => PLeaf(char_381.toString) }
      catPart_379 <- {
        def catPart_379_sub_385 = {
          val pos0_386 = mark
          val res_387 = for {
            catPart_388 <- {
              val pos_390 = mark
              val res_391 = EndOfLine()
              reset(pos_390)
              if (res_391.isSuccess) Failure(ParseFailed("Neglook failed", pos_390))
              else {
                Try(PEmpty)
              }
            }
            catPart_389 <- any.map { char_392 => PLeaf(char_392.toString) }
          } yield PBranch("catPart_379", Seq(catPart_388, catPart_389))
          res_387.recoverWith { case p: ParseError[Char] =>
            reset(pos0_386)
            Failure(p)
          }
        }

        var buf_382 = ArrayBuffer.empty[PTree]
        var pos_383 = mark
        var res_384 = catPart_379_sub_385
        res_384.recover { _ => reset(pos_383) }
        while (res_384.isSuccess) {
          buf_382 += res_384.get
          pos_383 = mark
          res_384 = catPart_379_sub_385
          res_384.recover { _ => reset(pos_383) }
        }
        Try(PBranch("catPart_379", buf_382.toSeq))
      }
      catPart_380 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_378, catPart_379, catPart_380))
    res_377.recoverWith { case p: ParseError[Char] =>
      reset(pos0_376)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_393 = mark
    expect(' ').map { char_394 => PLeaf(char_394.toString) }
      .recoverWith { case err_395: ParseError[Char] =>
        reset(pos_393)
        expect('\t').map { char_396 => PLeaf(char_396.toString) }
          .recoverWith { case err_397: ParseError[Char] =>
            reset(pos_393)
            EndOfLine().recoverWith { case err_398: ParseError[Char] =>
              reset(pos_393)
              Failure(err_395 ~ err_397 ~ err_398 ~ ParseFailed("", pos_393))
            }
          }
      }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_399 = mark
    val res_400 = {
      val pos_402 = mark
      val res_403 = for {
        char_part_404 <- expect('\r')
        char_part_405 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_404.toString), PLeaf(char_part_405.toString)))
      res_403.recoverWith { case p: ParseError[Char] =>
        reset(pos_402)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_402))
      }
    }
    res_400.recoverWith { case err_401: ParseError[Char] =>
      reset(pos_399)
      expect('\n').map { char_406 => PLeaf(char_406.toString) }
        .recoverWith { case err_407: ParseError[Char] =>
          reset(pos_399)
          expect('\r').map { char_408 => PLeaf(char_408.toString) }
            .recoverWith { case err_409: ParseError[Char] =>
              reset(pos_399)
              Failure(err_401 ~ err_407 ~ err_409 ~ ParseFailed("", pos_399))
            }
        }
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_410 = mark
    val res_411 = {
      val pos_412 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_412)
          Failure(p ~ ParseFailed("Expected any char", pos_412))
        }
    }
    reset(pos_410)
    if (res_411.isSuccess) Failure(ParseFailed("Neglook failed", pos_410))
    else {
      Try(PEmpty)
    }
  }
}
