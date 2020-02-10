package PEG.PEGParser

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
import PEG.lexparse.ParseError.implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer) {


  def Grammar(): Try[PTree] = {
    val pos0_2 = mark
    val res_3 = for {
      catPart_4 <- Spacing()
      catPart_5 <- {
        val pos0_7 = mark
        val res_8 = for {
          catPart_9 <- Definition()
          catPart_10 <- {
            def subMatch_14 = Definition()

            var buf_11 = ArrayBuffer.empty[PTree]
            var pos_12 = mark
            var res_13 = subMatch_14
            res_13.recover { _ => reset(pos_12) }
            while (res_13.isSuccess) {
              buf_11 += res_13.get
              pos_12 = mark
              res_13 = subMatch_14
              res_13.recover { _ => reset(pos_12) }
            }
            Try(PBranch("catPart_10", buf_11.toSeq))
          }
        } yield PBranch("catPart_5", Seq(catPart_9, catPart_10))
        res_8.recoverWith { case p: ParseError[Char] =>
          reset(pos0_7)
          Failure(p)
        }
      }
      catPart_6 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_4, catPart_5, catPart_6))
    res_3.recoverWith { case p: ParseError[Char] =>
      reset(pos0_2)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_16 = mark
    val res_17 = for {
      catPart_18 <- Identifier()
      catPart_19 <- {
        val pos_22 = mark
        STAR().recoverWith { case err_23: ParseError[Char] =>
          reset(pos_22)
          Try(PEmpty)
        }
      }
      catPart_20 <- LEFTARROW()
      catPart_21 <- Expression()
    } yield PBranch("Definition", Seq(catPart_18, catPart_19, catPart_20, catPart_21))
    res_17.recoverWith { case p: ParseError[Char] =>
      reset(pos0_16)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_25 = mark
    val res_26 = for {
      catPart_27 <- Sequence()
      catPart_28 <- {
        def subMatch_32 = {
          val pos0_33 = mark
          val res_34 = for {
            catPart_35 <- SLASH()
            catPart_36 <- Sequence()
          } yield PBranch("catPart_28", Seq(catPart_35, catPart_36))
          res_34.recoverWith { case p: ParseError[Char] =>
            reset(pos0_33)
            Failure(p)
          }
        }

        var buf_29 = ArrayBuffer.empty[PTree]
        var pos_30 = mark
        var res_31 = subMatch_32
        res_31.recover { _ => reset(pos_30) }
        while (res_31.isSuccess) {
          buf_29 += res_31.get
          pos_30 = mark
          res_31 = subMatch_32
          res_31.recover { _ => reset(pos_30) }
        }
        Try(PBranch("catPart_28", buf_29.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_27, catPart_28))
    res_26.recoverWith { case p: ParseError[Char] =>
      reset(pos0_25)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def subMatch_41 = Prefix()

    var buf_38 = ArrayBuffer.empty[PTree]
    var pos_39 = mark
    var res_40 = subMatch_41
    res_40.recover { _ => reset(pos_39) }
    while (res_40.isSuccess) {
      buf_38 += res_40.get
      pos_39 = mark
      res_40 = subMatch_41
      res_40.recover { _ => reset(pos_39) }
    }
    Try(PBranch("Sequence", buf_38.toSeq))
  }


  def Prefix(): Try[PTree] = {
    val pos0_43 = mark
    val res_44 = for {
      catPart_45 <- {
        val pos_47 = mark
        val res_48 = {
          val pos_50 = mark
          AND().recoverWith { case err_51: ParseError[Char] =>
            reset(pos_50)
            NOT().recoverWith { case err_52: ParseError[Char] =>
              reset(pos_50)
              Failure(err_51 ~ err_52 ~ ParseFailed("", pos_50))
            }
          }
        }
        res_48.recoverWith { case err_49: ParseError[Char] =>
          reset(pos_47)
          Try(PEmpty)
        }
      }
      catPart_46 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_45, catPart_46))
    res_44.recoverWith { case p: ParseError[Char] =>
      reset(pos0_43)
      Failure(p)
    }
  }


  def Suffix(): Try[PTree] = {
    val pos0_54 = mark
    val res_55 = for {
      catPart_56 <- Primary()
      catPart_57 <- {
        val pos_58 = mark
        val res_59 = {
          val pos_61 = mark
          QUESTION().recoverWith { case err_62: ParseError[Char] =>
            reset(pos_61)
            STAR().recoverWith { case err_63: ParseError[Char] =>
              reset(pos_61)
              PLUS().recoverWith { case err_64: ParseError[Char] =>
                reset(pos_61)
                Failure(err_62 ~ err_63 ~ err_64 ~ ParseFailed("", pos_61))
              }
            }
          }
        }
        res_59.recoverWith { case err_60: ParseError[Char] =>
          reset(pos_58)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_56, catPart_57))
    res_55.recoverWith { case p: ParseError[Char] =>
      reset(pos0_54)
      Failure(p)
    }
  }


  val cache_66 = mutable.HashMap.empty[Int, (Try[PTree], Int)]

  def Primary(): Try[PTree] = {
    def parser_65(): Try[PTree] = {
      val pos_70 = mark
      val res_71 = {
        val pos0_73 = mark
        val res_74 = for {
          catPart_75 <- Identifier()
          catPart_76 <- {
            val pos_77 = mark
            val res_78 = {
              val pos0_79 = mark
              val res_80 = for {
                catPart_81 <- {
                  val pos_83 = mark
                  STAR().recoverWith { case err_84: ParseError[Char] =>
                    reset(pos_83)
                    Try(PEmpty)
                  }
                }
                catPart_82 <- LEFTARROW()
              } yield PBranch("catPart_76", Seq(catPart_81, catPart_82))
              res_80.recoverWith { case p: ParseError[Char] =>
                reset(pos0_79)
                Failure(p)
              }
            }
            reset(pos_77)
            if (res_78.isSuccess) Failure(ParseFailed("Neglook failed", pos_77))
            else {
              Try(PEmpty)
            }
          }
        } yield PBranch("Primary", Seq(catPart_75, catPart_76))
        res_74.recoverWith { case p: ParseError[Char] =>
          reset(pos0_73)
          Failure(p)
        }
      }
      res_71.recoverWith { case err_72: ParseError[Char] =>
        reset(pos_70)
        val res_85 = {
          val pos0_87 = mark
          val res_88 = for {
            catPart_89 <- OPEN()
            catPart_90 <- Expression()
            catPart_91 <- CLOSE()
          } yield PBranch("Primary", Seq(catPart_89, catPart_90, catPart_91))
          res_88.recoverWith { case p: ParseError[Char] =>
            reset(pos0_87)
            Failure(p)
          }
        }
        res_85.recoverWith { case err_86: ParseError[Char] =>
          reset(pos_70)
          Literal().recoverWith { case err_92: ParseError[Char] =>
            reset(pos_70)
            Class().recoverWith { case err_93: ParseError[Char] =>
              reset(pos_70)
              DOT().recoverWith { case err_94: ParseError[Char] =>
                reset(pos_70)
                Failure(err_72 ~ err_86 ~ err_92 ~ err_93 ~ err_94 ~ ParseFailed("", pos_70))
              }
            }
          }
        }
      }
    }

    if (!cache_66.contains(mark)) {
      val init_68 = mark
      cache_66(init_68) = parser_65() -> mark
      reset(init_68)
    }
    val (res_67, pos_69) = cache_66(mark)
    reset(pos_69)
    res_67
  }


  def Identifier(): Try[PTree] = {
    val pos0_96 = mark
    val res_97 = for {
      catPart_98 <- IdentStart()
      catPart_99 <- {
        def subMatch_104 = IdentCont()

        var buf_101 = ArrayBuffer.empty[PTree]
        var pos_102 = mark
        var res_103 = subMatch_104
        res_103.recover { _ => reset(pos_102) }
        while (res_103.isSuccess) {
          buf_101 += res_103.get
          pos_102 = mark
          res_103 = subMatch_104
          res_103.recover { _ => reset(pos_102) }
        }
        Try(PBranch("catPart_99", buf_101.toSeq))
      }
      catPart_100 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_98, catPart_99, catPart_100))
    res_97.recoverWith { case p: ParseError[Char] =>
      reset(pos0_96)
      Failure(p)
    }
  }


  def IdentStart(): Try[PTree] = {
    val pos_106 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_107 => PLeaf(char_107.toString) }
      .recoverWith { case p: ParseError[Char] =>
        reset(pos_106)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_106))
      }
  }


  def IdentCont(): Try[PTree] = {
    val pos_109 = mark
    IdentStart().recoverWith { case err_110: ParseError[Char] =>
      reset(pos_109)
      expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
        .map { char_111 => PLeaf(char_111.toString) }
        .recoverWith { case err_112: ParseError[Char] =>
          reset(pos_109)
          Failure(err_110 ~ err_112 ~ ParseFailed("", pos_109))
        }
    }
  }


  def Literal(): Try[PTree] = {
    val pos_114 = mark
    val res_115 = {
      val pos0_117 = mark
      val res_118 = for {
        catPart_119 <- expect('\'').map { char_123 => PLeaf(char_123.toString) }
        catPart_120 <- {
          def subMatch_127 = {
            val pos0_128 = mark
            val res_129 = for {
              catPart_130 <- {
                val pos_132 = mark
                val res_133 = expect('\'')
                reset(pos_132)
                if (res_133.isSuccess) Failure(ParseFailed("Neglook failed", pos_132))
                else {
                  Try(PEmpty)
                }
              }
              catPart_131 <- Char()
            } yield PBranch("catPart_120", Seq(catPart_130, catPart_131))
            res_129.recoverWith { case p: ParseError[Char] =>
              reset(pos0_128)
              Failure(p)
            }
          }

          var buf_124 = ArrayBuffer.empty[PTree]
          var pos_125 = mark
          var res_126 = subMatch_127
          res_126.recover { _ => reset(pos_125) }
          while (res_126.isSuccess) {
            buf_124 += res_126.get
            pos_125 = mark
            res_126 = subMatch_127
            res_126.recover { _ => reset(pos_125) }
          }
          Try(PBranch("catPart_120", buf_124.toSeq))
        }
        catPart_121 <- expect('\'').map { char_134 => PLeaf(char_134.toString) }
        catPart_122 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_119, catPart_120, catPart_121, catPart_122))
      res_118.recoverWith { case p: ParseError[Char] =>
        reset(pos0_117)
        Failure(p)
      }
    }
    res_115.recoverWith { case err_116: ParseError[Char] =>
      reset(pos_114)
      val res_135 = {
        val pos0_137 = mark
        val res_138 = for {
          catPart_139 <- expect('"').map { char_143 => PLeaf(char_143.toString) }
          catPart_140 <- {
            def subMatch_147 = {
              val pos0_148 = mark
              val res_149 = for {
                catPart_150 <- {
                  val pos_152 = mark
                  val res_153 = expect('"')
                  reset(pos_152)
                  if (res_153.isSuccess) Failure(ParseFailed("Neglook failed", pos_152))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_151 <- Char()
              } yield PBranch("catPart_140", Seq(catPart_150, catPart_151))
              res_149.recoverWith { case p: ParseError[Char] =>
                reset(pos0_148)
                Failure(p)
              }
            }

            var buf_144 = ArrayBuffer.empty[PTree]
            var pos_145 = mark
            var res_146 = subMatch_147
            res_146.recover { _ => reset(pos_145) }
            while (res_146.isSuccess) {
              buf_144 += res_146.get
              pos_145 = mark
              res_146 = subMatch_147
              res_146.recover { _ => reset(pos_145) }
            }
            Try(PBranch("catPart_140", buf_144.toSeq))
          }
          catPart_141 <- expect('"').map { char_154 => PLeaf(char_154.toString) }
          catPart_142 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_139, catPart_140, catPart_141, catPart_142))
        res_138.recoverWith { case p: ParseError[Char] =>
          reset(pos0_137)
          Failure(p)
        }
      }
      res_135.recoverWith { case err_136: ParseError[Char] =>
        reset(pos_114)
        val res_155 = {
          val pos0_157 = mark
          val res_158 = for {
            catPart_159 <- expect('`').map { char_163 => PLeaf(char_163.toString) }
            catPart_160 <- {
              def subMatch_167 = {
                val pos0_168 = mark
                val res_169 = for {
                  catPart_170 <- {
                    val pos_172 = mark
                    val res_173 = expect('`')
                    reset(pos_172)
                    if (res_173.isSuccess) Failure(ParseFailed("Neglook failed", pos_172))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_171 <- Char()
                } yield PBranch("catPart_160", Seq(catPart_170, catPart_171))
                res_169.recoverWith { case p: ParseError[Char] =>
                  reset(pos0_168)
                  Failure(p)
                }
              }

              var buf_164 = ArrayBuffer.empty[PTree]
              var pos_165 = mark
              var res_166 = subMatch_167
              res_166.recover { _ => reset(pos_165) }
              while (res_166.isSuccess) {
                buf_164 += res_166.get
                pos_165 = mark
                res_166 = subMatch_167
                res_166.recover { _ => reset(pos_165) }
              }
              Try(PBranch("catPart_160", buf_164.toSeq))
            }
            catPart_161 <- expect('`').map { char_174 => PLeaf(char_174.toString) }
            catPart_162 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_159, catPart_160, catPart_161, catPart_162))
          res_158.recoverWith { case p: ParseError[Char] =>
            reset(pos0_157)
            Failure(p)
          }
        }
        res_155.recoverWith { case err_156: ParseError[Char] =>
          reset(pos_114)
          Failure(err_116 ~ err_136 ~ err_156 ~ ParseFailed("", pos_114))
        }
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_176 = mark
    val res_177 = for {
      catPart_178 <- expect('[').map { char_182 => PLeaf(char_182.toString) }
      catPart_179 <- {
        def subMatch_186 = {
          val pos0_187 = mark
          val res_188 = for {
            catPart_189 <- {
              val pos_191 = mark
              val res_192 = {
                val pos_193 = mark
                val res_194 = for {
                  char_part_195 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_195.toString)))
                res_194.recoverWith { case p: ParseError[Char] =>
                  reset(pos_193)
                  Failure(p ~ ParseFailed("expected ']'", pos_193))
                }
              }
              reset(pos_191)
              if (res_192.isSuccess) Failure(ParseFailed("Neglook failed", pos_191))
              else {
                Try(PEmpty)
              }
            }
            catPart_190 <- Range()
          } yield PBranch("catPart_179", Seq(catPart_189, catPart_190))
          res_188.recoverWith { case p: ParseError[Char] =>
            reset(pos0_187)
            Failure(p)
          }
        }

        var buf_183 = ArrayBuffer.empty[PTree]
        var pos_184 = mark
        var res_185 = subMatch_186
        res_185.recover { _ => reset(pos_184) }
        while (res_185.isSuccess) {
          buf_183 += res_185.get
          pos_184 = mark
          res_185 = subMatch_186
          res_185.recover { _ => reset(pos_184) }
        }
        Try(PBranch("catPart_179", buf_183.toSeq))
      }
      catPart_180 <- expect(']').map { char_196 => PLeaf(char_196.toString) }
      catPart_181 <- Spacing()
    } yield PBranch("Class", Seq(catPart_178, catPart_179, catPart_180, catPart_181))
    res_177.recoverWith { case p: ParseError[Char] =>
      reset(pos0_176)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_198 = mark
    val res_199 = {
      val pos0_201 = mark
      val res_202 = for {
        catPart_203 <- Char()
        catPart_204 <- expect('-').map { char_206 => PLeaf(char_206.toString) }
        catPart_205 <- Char()
      } yield PBranch("Range", Seq(catPart_203, catPart_204, catPart_205))
      res_202.recoverWith { case p: ParseError[Char] =>
        reset(pos0_201)
        Failure(p)
      }
    }
    res_199.recoverWith { case err_200: ParseError[Char] =>
      reset(pos_198)
      Char().recoverWith { case err_207: ParseError[Char] =>
        reset(pos_198)
        Failure(err_200 ~ err_207 ~ ParseFailed("", pos_198))
      }
    }
  }


  def Char(): Try[PTree] = {
    val pos_209 = mark
    val res_210 = {
      val pos0_212 = mark
      val res_213 = for {
        catPart_214 <- expect('\\').map { char_216 => PLeaf(char_216.toString) }
        catPart_215 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"').map { char_217 => PLeaf(char_217.toString) }
      } yield PBranch("Char", Seq(catPart_214, catPart_215))
      res_213.recoverWith { case p: ParseError[Char] =>
        reset(pos0_212)
        Failure(p)
      }
    }
    res_210.recoverWith { case err_211: ParseError[Char] =>
      reset(pos_209)
      val res_218 = {
        val pos0_220 = mark
        val res_221 = for {
          catPart_222 <- {
            val pos_224 = mark
            val res_225 = {
              val pos_226 = mark
              val res_227 = for {
                char_part_228 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_228.toString)))
              res_227.recoverWith { case p: ParseError[Char] =>
                reset(pos_226)
                Failure(p ~ ParseFailed("expected '\\'", pos_226))
              }
            }
            reset(pos_224)
            if (res_225.isSuccess) Failure(ParseFailed("Neglook failed", pos_224))
            else {
              Try(PEmpty)
            }
          }
          catPart_223 <- any.map { char_229 => PLeaf(char_229.toString) }
        } yield PBranch("Char", Seq(catPart_222, catPart_223))
        res_221.recoverWith { case p: ParseError[Char] =>
          reset(pos0_220)
          Failure(p)
        }
      }
      res_218.recoverWith { case err_219: ParseError[Char] =>
        reset(pos_209)
        Failure(err_211 ~ err_219 ~ ParseFailed("", pos_209))
      }
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_231 = mark
    val res_232 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_233 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_234 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_233, catPart_234))
    res_232.recoverWith { case p: ParseError[Char] =>
      reset(pos0_231)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_236 = mark
    val res_237 = for {
      catPart_238 <- expect('/').map { char_240 => PLeaf(char_240.toString) }
      catPart_239 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_238, catPart_239))
    res_237.recoverWith { case p: ParseError[Char] =>
      reset(pos0_236)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_242 = mark
    val res_243 = for {
      catPart_244 <- expect('&').map { char_246 => PLeaf(char_246.toString) }
      catPart_245 <- Spacing()
    } yield PBranch("AND", Seq(catPart_244, catPart_245))
    res_243.recoverWith { case p: ParseError[Char] =>
      reset(pos0_242)
      Failure(p)
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_248 = mark
    val res_249 = for {
      catPart_250 <- expect('!').map { char_252 => PLeaf(char_252.toString) }
      catPart_251 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_250, catPart_251))
    res_249.recoverWith { case p: ParseError[Char] =>
      reset(pos0_248)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_254 = mark
    val res_255 = for {
      catPart_256 <- expect('?').map { char_258 => PLeaf(char_258.toString) }
      catPart_257 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_256, catPart_257))
    res_255.recoverWith { case p: ParseError[Char] =>
      reset(pos0_254)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_260 = mark
    val res_261 = for {
      catPart_262 <- expect('*').map { char_264 => PLeaf(char_264.toString) }
      catPart_263 <- Spacing()
    } yield PBranch("STAR", Seq(catPart_262, catPart_263))
    res_261.recoverWith { case p: ParseError[Char] =>
      reset(pos0_260)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_266 = mark
    val res_267 = for {
      catPart_268 <- expect('+').map { char_270 => PLeaf(char_270.toString) }
      catPart_269 <- Spacing()
    } yield PBranch("PLUS", Seq(catPart_268, catPart_269))
    res_267.recoverWith { case p: ParseError[Char] =>
      reset(pos0_266)
      Failure(p)
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_272 = mark
    val res_273 = for {
      catPart_274 <- expect('(').map { char_276 => PLeaf(char_276.toString) }
      catPart_275 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_274, catPart_275))
    res_273.recoverWith { case p: ParseError[Char] =>
      reset(pos0_272)
      Failure(p)
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_278 = mark
    val res_279 = for {
      catPart_280 <- expect(')').map { char_282 => PLeaf(char_282.toString) }
      catPart_281 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_280, catPart_281))
    res_279.recoverWith { case p: ParseError[Char] =>
      reset(pos0_278)
      Failure(p)
    }
  }


  def DOT(): Try[PTree] = {
    val pos0_284 = mark
    val res_285 = for {
      catPart_286 <- expect('.').map { char_288 => PLeaf(char_288.toString) }
      catPart_287 <- Spacing()
    } yield PBranch("DOT", Seq(catPart_286, catPart_287))
    res_285.recoverWith { case p: ParseError[Char] =>
      reset(pos0_284)
      Failure(p)
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_293 = {
      val pos_294 = mark
      Space().recoverWith { case err_295: ParseError[Char] =>
        reset(pos_294)
        Comment().recoverWith { case err_296: ParseError[Char] =>
          reset(pos_294)
          Failure(err_295 ~ err_296 ~ ParseFailed("", pos_294))
        }
      }
    }

    var buf_290 = ArrayBuffer.empty[PTree]
    var pos_291 = mark
    var res_292 = subMatch_293
    res_292.recover { _ => reset(pos_291) }
    while (res_292.isSuccess) {
      buf_290 += res_292.get
      pos_291 = mark
      res_292 = subMatch_293
      res_292.recover { _ => reset(pos_291) }
    }
    Try(PBranch("Spacing", buf_290.toSeq))
  }


  def Comment(): Try[PTree] = {
    val pos0_298 = mark
    val res_299 = for {
      catPart_300 <- expect('#').map { char_303 => PLeaf(char_303.toString) }
      catPart_301 <- {
        def subMatch_307 = {
          val pos0_308 = mark
          val res_309 = for {
            catPart_310 <- {
              val pos_312 = mark
              val res_313 = EndOfLine()
              reset(pos_312)
              if (res_313.isSuccess) Failure(ParseFailed("Neglook failed", pos_312))
              else {
                Try(PEmpty)
              }
            }
            catPart_311 <- any.map { char_314 => PLeaf(char_314.toString) }
          } yield PBranch("catPart_301", Seq(catPart_310, catPart_311))
          res_309.recoverWith { case p: ParseError[Char] =>
            reset(pos0_308)
            Failure(p)
          }
        }

        var buf_304 = ArrayBuffer.empty[PTree]
        var pos_305 = mark
        var res_306 = subMatch_307
        res_306.recover { _ => reset(pos_305) }
        while (res_306.isSuccess) {
          buf_304 += res_306.get
          pos_305 = mark
          res_306 = subMatch_307
          res_306.recover { _ => reset(pos_305) }
        }
        Try(PBranch("catPart_301", buf_304.toSeq))
      }
      catPart_302 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_300, catPart_301, catPart_302))
    res_299.recoverWith { case p: ParseError[Char] =>
      reset(pos0_298)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_316 = mark
    expect(' ').map { char_317 => PLeaf(char_317.toString) }
      .recoverWith { case err_318: ParseError[Char] =>
        reset(pos_316)
        expect('\t').map { char_319 => PLeaf(char_319.toString) }
          .recoverWith { case err_320: ParseError[Char] =>
            reset(pos_316)
            EndOfLine().recoverWith { case err_321: ParseError[Char] =>
              reset(pos_316)
              Failure(err_318 ~ err_320 ~ err_321 ~ ParseFailed("", pos_316))
            }
          }
      }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_323 = mark
    val res_324 = {
      val pos_326 = mark
      val res_327 = for {
        char_part_328 <- expect('\r')
        char_part_329 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_328.toString), PLeaf(char_part_329.toString)))
      res_327.recoverWith { case p: ParseError[Char] =>
        reset(pos_326)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_326))
      }
    }
    res_324.recoverWith { case err_325: ParseError[Char] =>
      reset(pos_323)
      expect('\n').map { char_330 => PLeaf(char_330.toString) }
        .recoverWith { case err_331: ParseError[Char] =>
          reset(pos_323)
          expect('\r').map { char_332 => PLeaf(char_332.toString) }
            .recoverWith { case err_333: ParseError[Char] =>
              reset(pos_323)
              Failure(err_325 ~ err_331 ~ err_333 ~ ParseFailed("", pos_323))
            }
        }
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_335 = mark
    val res_336 = {
      val pos_337 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_337)
          Failure(p ~ ParseFailed("Expected any char", pos_337))
        }
    }
    reset(pos_335)
    if (res_336.isSuccess) Failure(ParseFailed("Neglook failed", pos_335))
    else {
      Try(PEmpty)
    }
  }
}
