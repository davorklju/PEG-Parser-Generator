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
                val res_35 = {
                  val pos_36 = mark
                  expect('\'')
                    .map { char_37 => PLeaf(char_37.toString) }
                    .recoverWith { case p: ParseError =>
                      reset(pos_36)
                      Failure(p ~ ParseFailed("Expected one of '\''", pos_36))
                    }
                }
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

          var parts_26 = ArrayBuffer.empty[PTree]
          var pos_27 = mark
          var res_28 = subMatch_29
          res_28.recover { _ => reset(pos_27) }
          while (res_28.isSuccess) {
            parts_26 += res_28.get
            pos_27 = mark
            res_28 = subMatch_29
            res_28.recover { _ => reset(pos_27) }
          }
          Try(PBranch("catPart_22", parts_26.toSeq))
        }
        catPart_23 <- expect('\'').map { char_38 => PLeaf(char_38.toString) }
        catPart_24 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_21, catPart_22, catPart_23, catPart_24))
      res_20.recoverWith { case p: ParseError =>
        reset(pos0_19)
        Failure(p)
      }
    }
    res_17.recoverWith { case err_18: ParseError =>
      reset(pos_16)
      val res_39 = {
        val pos0_41 = mark
        val res_42 = for {
          catPart_43 <- expect('"').map { char_47 => PLeaf(char_47.toString) }
          catPart_44 <- {
            def subMatch_51 = {
              val pos0_52 = mark
              val res_53 = for {
                catPart_54 <- {
                  val pos_56 = mark
                  val res_57 = {
                    val pos_58 = mark
                    expect('"')
                      .map { char_59 => PLeaf(char_59.toString) }
                      .recoverWith { case p: ParseError =>
                        reset(pos_58)
                        Failure(p ~ ParseFailed("Expected one of \"", pos_58))
                      }
                  }
                  reset(pos_56)
                  if (res_57.isSuccess) Failure(ParseFailed("Neglook failed", pos_56))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_55 <- Char()
              } yield PBranch("catPart_44", Seq(catPart_54, catPart_55))
              res_53.recoverWith { case p: ParseError =>
                reset(pos0_52)
                Failure(p)
              }
            }

            var parts_48 = ArrayBuffer.empty[PTree]
            var pos_49 = mark
            var res_50 = subMatch_51
            res_50.recover { _ => reset(pos_49) }
            while (res_50.isSuccess) {
              parts_48 += res_50.get
              pos_49 = mark
              res_50 = subMatch_51
              res_50.recover { _ => reset(pos_49) }
            }
            Try(PBranch("catPart_44", parts_48.toSeq))
          }
          catPart_45 <- expect('"').map { char_60 => PLeaf(char_60.toString) }
          catPart_46 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_43, catPart_44, catPart_45, catPart_46))
        res_42.recoverWith { case p: ParseError =>
          reset(pos0_41)
          Failure(p)
        }
      }
      res_39.recoverWith { case err_40: ParseError =>
        reset(pos_16)
        val res_61 = {
          val pos0_63 = mark
          val res_64 = for {
            catPart_65 <- expect('`').map { char_69 => PLeaf(char_69.toString) }
            catPart_66 <- {
              def subMatch_73 = {
                val pos0_74 = mark
                val res_75 = for {
                  catPart_76 <- {
                    val pos_78 = mark
                    val res_79 = {
                      val pos_80 = mark
                      expect('`')
                        .map { char_81 => PLeaf(char_81.toString) }
                        .recoverWith { case p: ParseError =>
                          reset(pos_80)
                          Failure(p ~ ParseFailed("Expected one of '`'", pos_80))
                        }
                    }
                    reset(pos_78)
                    if (res_79.isSuccess) Failure(ParseFailed("Neglook failed", pos_78))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_77 <- Char()
                } yield PBranch("catPart_66", Seq(catPart_76, catPart_77))
                res_75.recoverWith { case p: ParseError =>
                  reset(pos0_74)
                  Failure(p)
                }
              }

              var parts_70 = ArrayBuffer.empty[PTree]
              var pos_71 = mark
              var res_72 = subMatch_73
              res_72.recover { _ => reset(pos_71) }
              while (res_72.isSuccess) {
                parts_70 += res_72.get
                pos_71 = mark
                res_72 = subMatch_73
                res_72.recover { _ => reset(pos_71) }
              }
              Try(PBranch("catPart_66", parts_70.toSeq))
            }
            catPart_67 <- expect('`').map { char_82 => PLeaf(char_82.toString) }
            catPart_68 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_65, catPart_66, catPart_67, catPart_68))
          res_64.recoverWith { case p: ParseError =>
            reset(pos0_63)
            Failure(p)
          }
        }
        res_61.recoverWith { case err_62: ParseError =>
          reset(pos_16)
          Failure(err_18 ~ err_40 ~ err_62 ~ ParseFailed("", pos_16))
        }
      }
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_83 = mark
    val res_84 = for {
      catPart_85 <- expect(')').map { char_87 => PLeaf(char_87.toString) }
      catPart_86 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_85, catPart_86))
    res_84.recoverWith { case p: ParseError =>
      reset(pos0_83)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_88 = mark
    val res_89 = for {
      catPart_90 <- Identifier()
      catPart_91 <- LEFTARROW()
      catPart_92 <- Expression()
    } yield PBranch("Definition", Seq(catPart_90, catPart_91, catPart_92))
    res_89.recoverWith { case p: ParseError =>
      reset(pos0_88)
      Failure(p)
    }
  }


  def Char(): Try[PTree] = {
    val pos_93 = mark
    val res_94 = {
      val pos0_96 = mark
      val res_97 = for {
        catPart_98 <- expect('\\').map { char_100 => PLeaf(char_100.toString) }
        catPart_99 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"').map { char_101 => PLeaf(char_101.toString) }
      } yield PBranch("Char", Seq(catPart_98, catPart_99))
      res_97.recoverWith { case p: ParseError =>
        reset(pos0_96)
        Failure(p)
      }
    }
    res_94.recoverWith { case err_95: ParseError =>
      reset(pos_93)
      val res_102 = {
        val pos0_104 = mark
        val res_105 = for {
          catPart_106 <- {
            val pos_108 = mark
            val res_109 = {
              val pos_110 = mark
              val res_111 = for {
                char_part_112 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_112.toString)))
              res_111.recoverWith { case p: ParseError =>
                reset(pos_110)
                Failure(p ~ ParseFailed("expected '\\'", pos_110))
              }
            }
            reset(pos_108)
            if (res_109.isSuccess) Failure(ParseFailed("Neglook failed", pos_108))
            else {
              Try(PEmpty)
            }
          }
          catPart_107 <- {
            val pos_113 = mark
            any.map { x => PLeaf(x.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_113)
                Failure(p ~ ParseFailed("Expected any char", pos_113))
              }
          }
        } yield PBranch("Char", Seq(catPart_106, catPart_107))
        res_105.recoverWith { case p: ParseError =>
          reset(pos0_104)
          Failure(p)
        }
      }
      res_102.recoverWith { case err_103: ParseError =>
        reset(pos_93)
        Failure(err_95 ~ err_103 ~ ParseFailed("", pos_93))
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_114 = mark
    val res_115 = for {
      catPart_116 <- expect('[').map { char_120 => PLeaf(char_120.toString) }
      catPart_117 <- {
        def subMatch_124 = {
          val pos0_125 = mark
          val res_126 = for {
            catPart_127 <- {
              val pos_129 = mark
              val res_130 = {
                val pos_131 = mark
                val res_132 = for {
                  char_part_133 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_133.toString)))
                res_132.recoverWith { case p: ParseError =>
                  reset(pos_131)
                  Failure(p ~ ParseFailed("expected ']'", pos_131))
                }
              }
              reset(pos_129)
              if (res_130.isSuccess) Failure(ParseFailed("Neglook failed", pos_129))
              else {
                Try(PEmpty)
              }
            }
            catPart_128 <- Range()
          } yield PBranch("catPart_117", Seq(catPart_127, catPart_128))
          res_126.recoverWith { case p: ParseError =>
            reset(pos0_125)
            Failure(p)
          }
        }

        var parts_121 = ArrayBuffer.empty[PTree]
        var pos_122 = mark
        var res_123 = subMatch_124
        res_123.recover { _ => reset(pos_122) }
        while (res_123.isSuccess) {
          parts_121 += res_123.get
          pos_122 = mark
          res_123 = subMatch_124
          res_123.recover { _ => reset(pos_122) }
        }
        Try(PBranch("catPart_117", parts_121.toSeq))
      }
      catPart_118 <- expect(']').map { char_134 => PLeaf(char_134.toString) }
      catPart_119 <- Spacing()
    } yield PBranch("Class", Seq(catPart_116, catPart_117, catPart_118, catPart_119))
    res_115.recoverWith { case p: ParseError =>
      reset(pos0_114)
      Failure(p)
    }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_135 = mark
    val res_136 = {
      val pos_138 = mark
      val res_139 = for {
        char_part_140 <- expect('\r')
        char_part_141 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_140.toString), PLeaf(char_part_141.toString)))
      res_139.recoverWith { case p: ParseError =>
        reset(pos_138)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_138))
      }
    }
    res_136.recoverWith { case err_137: ParseError =>
      reset(pos_135)
      val res_142 = {
        val pos_144 = mark
        val res_145 = for {
          char_part_146 <- expect('\n')
        } yield PBranch("Lit", Seq(PLeaf(char_part_146.toString)))
        res_145.recoverWith { case p: ParseError =>
          reset(pos_144)
          Failure(p ~ ParseFailed("expected '\n'", pos_144))
        }
      }
      res_142.recoverWith { case err_143: ParseError =>
        reset(pos_135)
        val res_147 = {
          val pos_149 = mark
          val res_150 = for {
            char_part_151 <- expect('\r')
          } yield PBranch("Lit", Seq(PLeaf(char_part_151.toString)))
          res_150.recoverWith { case p: ParseError =>
            reset(pos_149)
            Failure(p ~ ParseFailed("expected '\r'", pos_149))
          }
        }
        res_147.recoverWith { case err_148: ParseError =>
          reset(pos_135)
          Failure(err_137 ~ err_143 ~ err_148 ~ ParseFailed("", pos_135))
        }
      }
    }
  }


  def IdentCont(): Try[PTree] = {
    val pos_152 = mark
    IdentStart().recoverWith { case err_153: ParseError =>
      reset(pos_152)
      val res_154 = {
        val pos_156 = mark
        expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
          .map { char_157 => PLeaf(char_157.toString) }
          .recoverWith { case p: ParseError =>
            reset(pos_156)
            Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_156))
          }
      }
      res_154.recoverWith { case err_155: ParseError =>
        reset(pos_152)
        Failure(err_153 ~ err_155 ~ ParseFailed("", pos_152))
      }
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_158 = mark
    val res_159 = for {
      catPart_160 <- expect('!').map { char_162 => PLeaf(char_162.toString) }
      catPart_161 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_160, catPart_161))
    res_159.recoverWith { case p: ParseError =>
      reset(pos0_158)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_163 = mark
    val res_164 = for {
      catPart_165 <- expect('?').map { char_167 => PLeaf(char_167.toString) }
      catPart_166 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_165, catPart_166))
    res_164.recoverWith { case p: ParseError =>
      reset(pos0_163)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_168 = mark
    val res_169 = for {
      catPart_170 <- Sequence()
      catPart_171 <- {
        def subMatch_175 = {
          val pos0_176 = mark
          val res_177 = for {
            catPart_178 <- SLASH()
            catPart_179 <- Sequence()
          } yield PBranch("catPart_171", Seq(catPart_178, catPart_179))
          res_177.recoverWith { case p: ParseError =>
            reset(pos0_176)
            Failure(p)
          }
        }

        var parts_172 = ArrayBuffer.empty[PTree]
        var pos_173 = mark
        var res_174 = subMatch_175
        res_174.recover { _ => reset(pos_173) }
        while (res_174.isSuccess) {
          parts_172 += res_174.get
          pos_173 = mark
          res_174 = subMatch_175
          res_174.recover { _ => reset(pos_173) }
        }
        Try(PBranch("catPart_171", parts_172.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_170, catPart_171))
    res_169.recoverWith { case p: ParseError =>
      reset(pos0_168)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    def subMatch_183 = {
      val pos_184 = mark
      Prefix().recoverWith { case p: ParseError =>
        reset(pos_184)
        Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_184))
      }
    }

    var parts_180 = ArrayBuffer.empty[PTree]
    var pos_181 = mark
    var res_182 = subMatch_183
    res_182.recover { _ => reset(pos_181) }
    while (res_182.isSuccess) {
      parts_180 += res_182.get
      pos_181 = mark
      res_182 = subMatch_183
      res_182.recover { _ => reset(pos_181) }
    }
    Try(PBranch("Sequence", parts_180.toSeq))
  }


  def Suffix(): Try[PTree] = {
    val pos0_185 = mark
    val res_186 = for {
      catPart_187 <- Primary()
      catPart_188 <- {
        val pos_189 = mark
        val res_190 = {
          val pos_192 = mark
          QUESTION().recoverWith { case err_193: ParseError =>
            reset(pos_192)
            STAR().recoverWith { case err_194: ParseError =>
              reset(pos_192)
              PLUS().recoverWith { case err_195: ParseError =>
                reset(pos_192)
                Failure(err_193 ~ err_194 ~ err_195 ~ ParseFailed("", pos_192))
              }
            }
          }
        }
        res_190.recoverWith { case err_191: ParseError =>
          reset(pos_189)
          Try(PEmpty)
        }
      }
    } yield PBranch("Suffix", Seq(catPart_187, catPart_188))
    res_186.recoverWith { case p: ParseError =>
      reset(pos0_185)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_196 = mark
    val res_197 = {
      val pos_199 = mark
      val res_200 = for {
        char_part_201 <- expect(' ')
      } yield PBranch("Lit", Seq(PLeaf(char_part_201.toString)))
      res_200.recoverWith { case p: ParseError =>
        reset(pos_199)
        Failure(p ~ ParseFailed("expected ' '", pos_199))
      }
    }
    res_197.recoverWith { case err_198: ParseError =>
      reset(pos_196)
      val res_202 = {
        val pos_204 = mark
        val res_205 = for {
          char_part_206 <- expect('\t')
        } yield PBranch("Lit", Seq(PLeaf(char_part_206.toString)))
        res_205.recoverWith { case p: ParseError =>
          reset(pos_204)
          Failure(p ~ ParseFailed("expected '\t'", pos_204))
        }
      }
      res_202.recoverWith { case err_203: ParseError =>
        reset(pos_196)
        EndOfLine().recoverWith { case err_207: ParseError =>
          reset(pos_196)
          Failure(err_198 ~ err_203 ~ err_207 ~ ParseFailed("", pos_196))
        }
      }
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_208 = mark
    val res_209 = for {
      catPart_210 <- expect('(').map { char_212 => PLeaf(char_212.toString) }
      catPart_211 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_210, catPart_211))
    res_209.recoverWith { case p: ParseError =>
      reset(pos0_208)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_213 = mark
    val res_214 = {
      val pos_215 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_215)
          Failure(p ~ ParseFailed("Expected any char", pos_215))
        }
    }
    reset(pos_213)
    if (res_214.isSuccess) Failure(ParseFailed("Neglook failed", pos_213))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_216 = mark
    val res_217 = {
      val pos0_219 = mark
      val res_220 = for {
        catPart_221 <- Identifier()
        catPart_222 <- {
          val pos_223 = mark
          val res_224 = {
            val pos_225 = mark
            LEFTARROW().recoverWith { case p: ParseError =>
              reset(pos_225)
              Failure(p ~ ParseFailed("expected Var 'LEFTARROW'", pos_225))
            }
          }
          reset(pos_223)
          if (res_224.isSuccess) Failure(ParseFailed("Neglook failed", pos_223))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_221, catPart_222))
      res_220.recoverWith { case p: ParseError =>
        reset(pos0_219)
        Failure(p)
      }
    }
    res_217.recoverWith { case err_218: ParseError =>
      reset(pos_216)
      val res_226 = {
        val pos0_228 = mark
        val res_229 = for {
          catPart_230 <- OPEN()
          catPart_231 <- Expression()
          catPart_232 <- CLOSE()
        } yield PBranch("Primary", Seq(catPart_230, catPart_231, catPart_232))
        res_229.recoverWith { case p: ParseError =>
          reset(pos0_228)
          Failure(p)
        }
      }
      res_226.recoverWith { case err_227: ParseError =>
        reset(pos_216)
        Literal().recoverWith { case err_233: ParseError =>
          reset(pos_216)
          Class().recoverWith { case err_234: ParseError =>
            reset(pos_216)
            DOT().recoverWith { case err_235: ParseError =>
              reset(pos_216)
              Failure(err_218 ~ err_227 ~ err_233 ~ err_234 ~ err_235 ~ ParseFailed("", pos_216))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    def subMatch_239 = {
      val pos_240 = mark
      Space().recoverWith { case err_241: ParseError =>
        reset(pos_240)
        Comment().recoverWith { case err_242: ParseError =>
          reset(pos_240)
          Failure(err_241 ~ err_242 ~ ParseFailed("", pos_240))
        }
      }
    }

    var parts_236 = ArrayBuffer.empty[PTree]
    var pos_237 = mark
    var res_238 = subMatch_239
    res_238.recover { _ => reset(pos_237) }
    while (res_238.isSuccess) {
      parts_236 += res_238.get
      pos_237 = mark
      res_238 = subMatch_239
      res_238.recover { _ => reset(pos_237) }
    }
    Try(PBranch("Spacing", parts_236.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_243 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_244 => PLeaf(char_244.toString) }
      .recoverWith { case p: ParseError =>
        reset(pos_243)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_243))
      }
  }


  def Prefix(): Try[PTree] = {
    val pos0_245 = mark
    val res_246 = for {
      catPart_247 <- {
        val pos_249 = mark
        val res_250 = {
          val pos_252 = mark
          AND().recoverWith { case err_253: ParseError =>
            reset(pos_252)
            NOT().recoverWith { case err_254: ParseError =>
              reset(pos_252)
              Failure(err_253 ~ err_254 ~ ParseFailed("", pos_252))
            }
          }
        }
        res_250.recoverWith { case err_251: ParseError =>
          reset(pos_249)
          Try(PEmpty)
        }
      }
      catPart_248 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_247, catPart_248))
    res_246.recoverWith { case p: ParseError =>
      reset(pos0_245)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_255 = mark
    val res_256 = for {
      catPart_257 <- IdentStart()
      catPart_258 <- {
        def subMatch_263 = {
          val pos_264 = mark
          IdentCont().recoverWith { case p: ParseError =>
            reset(pos_264)
            Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_264))
          }
        }

        var parts_260 = ArrayBuffer.empty[PTree]
        var pos_261 = mark
        var res_262 = subMatch_263
        res_262.recover { _ => reset(pos_261) }
        while (res_262.isSuccess) {
          parts_260 += res_262.get
          pos_261 = mark
          res_262 = subMatch_263
          res_262.recover { _ => reset(pos_261) }
        }
        Try(PBranch("catPart_258", parts_260.toSeq))
      }
      catPart_259 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_257, catPart_258, catPart_259))
    res_256.recoverWith { case p: ParseError =>
      reset(pos0_255)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_265 = mark
    val res_266 = for {
      catPart_267 <- expect('/').map { char_269 => PLeaf(char_269.toString) }
      catPart_268 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_267, catPart_268))
    res_266.recoverWith { case p: ParseError =>
      reset(pos0_265)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_270 = mark
    val res_271 = for {
      _ <- expect('<')
      _ <- expect('-')
      catPart_272 <- Try(PBranch("Lit", Seq(PLeaf('<'.toString), PLeaf('-'.toString))))
      catPart_273 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_272, catPart_273))
    res_271.recoverWith { case p: ParseError =>
      reset(pos0_270)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_274 = mark
    val res_275 = for {
      catPart_276 <- expect('#').map { char_279 => PLeaf(char_279.toString) }
      catPart_277 <- {
        def subMatch_283 = {
          val pos0_284 = mark
          val res_285 = for {
            catPart_286 <- {
              val pos_288 = mark
              val res_289 = {
                val pos_290 = mark
                EndOfLine().recoverWith { case p: ParseError =>
                  reset(pos_290)
                  Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_290))
                }
              }
              reset(pos_288)
              if (res_289.isSuccess) Failure(ParseFailed("Neglook failed", pos_288))
              else {
                Try(PEmpty)
              }
            }
            catPart_287 <- {
              val pos_291 = mark
              any.map { x => PLeaf(x.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_291)
                  Failure(p ~ ParseFailed("Expected any char", pos_291))
                }
            }
          } yield PBranch("catPart_277", Seq(catPart_286, catPart_287))
          res_285.recoverWith { case p: ParseError =>
            reset(pos0_284)
            Failure(p)
          }
        }

        var parts_280 = ArrayBuffer.empty[PTree]
        var pos_281 = mark
        var res_282 = subMatch_283
        res_282.recover { _ => reset(pos_281) }
        while (res_282.isSuccess) {
          parts_280 += res_282.get
          pos_281 = mark
          res_282 = subMatch_283
          res_282.recover { _ => reset(pos_281) }
        }
        Try(PBranch("catPart_277", parts_280.toSeq))
      }
      catPart_278 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_276, catPart_277, catPart_278))
    res_275.recoverWith { case p: ParseError =>
      reset(pos0_274)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_292 = mark
    val res_293 = for {
      catPart_294 <- expect('&').map { char_296 => PLeaf(char_296.toString) }
      catPart_295 <- Spacing()
    } yield PBranch("AND", Seq(catPart_294, catPart_295))
    res_293.recoverWith { case p: ParseError =>
      reset(pos0_292)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_297 = mark
    val res_298 = {
      val pos0_300 = mark
      val res_301 = for {
        catPart_302 <- Char()
        catPart_303 <- expect('-').map { char_305 => PLeaf(char_305.toString) }
        catPart_304 <- Char()
      } yield PBranch("Range", Seq(catPart_302, catPart_303, catPart_304))
      res_301.recoverWith { case p: ParseError =>
        reset(pos0_300)
        Failure(p)
      }
    }
    res_298.recoverWith { case err_299: ParseError =>
      reset(pos_297)
      Char().recoverWith { case err_306: ParseError =>
        reset(pos_297)
        Failure(err_299 ~ err_306 ~ ParseFailed("", pos_297))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_307 = mark
    val res_308 = for {
      catPart_309 <- Spacing()
      catPart_310 <- {
        val pos0_312 = mark
        val res_313 = for {
          catPart_314 <- Definition()
          catPart_315 <- {
            def subMatch_319 = {
              val pos_320 = mark
              Definition().recoverWith { case p: ParseError =>
                reset(pos_320)
                Failure(p ~ ParseFailed("expected Var 'Definition'", pos_320))
              }
            }

            var parts_316 = ArrayBuffer.empty[PTree]
            var pos_317 = mark
            var res_318 = subMatch_319
            res_318.recover { _ => reset(pos_317) }
            while (res_318.isSuccess) {
              parts_316 += res_318.get
              pos_317 = mark
              res_318 = subMatch_319
              res_318.recover { _ => reset(pos_317) }
            }
            Try(PBranch("catPart_315", parts_316.toSeq))
          }
        } yield PBranch("catPart_310", Seq(catPart_314, catPart_315))
        res_313.recoverWith { case p: ParseError =>
          reset(pos0_312)
          Failure(p)
        }
      }
      catPart_311 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_309, catPart_310, catPart_311))
    res_308.recoverWith { case p: ParseError =>
      reset(pos0_307)
      Failure(p)
    }
  }
}
