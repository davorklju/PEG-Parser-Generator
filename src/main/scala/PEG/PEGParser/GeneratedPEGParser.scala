package PEG.PEGParser

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class GeneratedPEGParser(lexer: Lexer) extends Parser(lexer) {


  def DOT(): Try[PTree] = {
    val pos0_1 = mark
    val res_2 = for {
      catPart_3 <- {
        val pos_5 = mark
        val res_6 = for {
          char_part_7 <- expect('.')
        } yield PBranch("Lit", Seq(PLeaf(char_part_7.toString)))
        res_6.recoverWith { case p: ParseError =>
          reset(pos_5)
          Failure(p ~ ParseFailed("expected '.'", pos_5))
        }
      }
      catPart_4 <- Spacing()
    } yield PBranch("DOT", Seq(catPart_3, catPart_4))
    res_2.recoverWith { case p: ParseError =>
      reset(pos0_1)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_8 = mark
    val res_9 = for {
      catPart_10 <- {
        val pos_12 = mark
        val res_13 = for {
          char_part_14 <- expect('*')
        } yield PBranch("Lit", Seq(PLeaf(char_part_14.toString)))
        res_13.recoverWith { case p: ParseError =>
          reset(pos_12)
          Failure(p ~ ParseFailed("expected '*'", pos_12))
        }
      }
      catPart_11 <- Spacing()
    } yield PBranch("STAR", Seq(catPart_10, catPart_11))
    res_9.recoverWith { case p: ParseError =>
      reset(pos0_8)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_15 = mark
    val res_16 = for {
      catPart_17 <- {
        val pos_19 = mark
        val res_20 = for {
          char_part_21 <- expect('+')
        } yield PBranch("Lit", Seq(PLeaf(char_part_21.toString)))
        res_20.recoverWith { case p: ParseError =>
          reset(pos_19)
          Failure(p ~ ParseFailed("expected '+'", pos_19))
        }
      }
      catPart_18 <- Spacing()
    } yield PBranch("PLUS", Seq(catPart_17, catPart_18))
    res_16.recoverWith { case p: ParseError =>
      reset(pos0_15)
      Failure(p)
    }
  }


  def Literal(): Try[PTree] = {
    val pos_22 = mark
    val res_23 = {
      val pos0_25 = mark
      val res_26 = for {
        catPart_27 <- {
          val pos_31 = mark
          expect('\'')
            .map { char_32 => PLeaf(char_32.toString) }
            .recoverWith { case p: ParseError =>
              reset(pos_31)
              Failure(p ~ ParseFailed("Expected one of '\''", pos_31))
            }
        }
        catPart_28 <- {
          var parts_33 = ArrayBuffer.empty[PTree]
          var pos_34 = mark
          var res_35 = {
            val pos0_36 = mark
            val res_37 = for {
              catPart_38 <- {
                val pos_40 = mark
                val res_41 = {
                  val pos_42 = mark
                  expect('\'')
                    .map { char_43 => PLeaf(char_43.toString) }
                    .recoverWith { case p: ParseError =>
                      reset(pos_42)
                      Failure(p ~ ParseFailed("Expected one of '\''", pos_42))
                    }
                }
                reset(pos_40)
                if (res_41.isSuccess) Failure(ParseFailed("Neglook failed", pos_40))
                else {
                  Try(PEmpty)
                }
              }
              catPart_39 <- Char()
            } yield PBranch("catPart_28", Seq(catPart_38, catPart_39))
            res_37.recoverWith { case p: ParseError =>
              reset(pos0_36)
              Failure(p)
            }
          }
          res_35.recover { _ => reset(pos_34) }
          while (res_35.isSuccess) {
            parts_33 += res_35.get
            pos_34 = mark
            res_35 = {
              val pos0_44 = mark
              val res_45 = for {
                catPart_46 <- {
                  val pos_48 = mark
                  val res_49 = {
                    val pos_50 = mark
                    expect('\'')
                      .map { char_51 => PLeaf(char_51.toString) }
                      .recoverWith { case p: ParseError =>
                        reset(pos_50)
                        Failure(p ~ ParseFailed("Expected one of '\''", pos_50))
                      }
                  }
                  reset(pos_48)
                  if (res_49.isSuccess) Failure(ParseFailed("Neglook failed", pos_48))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_47 <- Char()
              } yield PBranch("catPart_28", Seq(catPart_46, catPart_47))
              res_45.recoverWith { case p: ParseError =>
                reset(pos0_44)
                Failure(p)
              }
            }
            res_35.recover { _ => reset(pos_34) }
          }
          Try(PBranch("catPart_28", parts_33.toSeq))
        }
        catPart_29 <- {
          val pos_52 = mark
          expect('\'')
            .map { char_53 => PLeaf(char_53.toString) }
            .recoverWith { case p: ParseError =>
              reset(pos_52)
              Failure(p ~ ParseFailed("Expected one of '\''", pos_52))
            }
        }
        catPart_30 <- Spacing()
      } yield PBranch("Literal", Seq(catPart_27, catPart_28, catPart_29, catPart_30))
      res_26.recoverWith { case p: ParseError =>
        reset(pos0_25)
        Failure(p)
      }
    }
    res_23.recoverWith { case err_24: ParseError =>
      reset(pos_22)
      val res_54 = {
        val pos0_56 = mark
        val res_57 = for {
          catPart_58 <- {
            val pos_62 = mark
            expect('"')
              .map { char_63 => PLeaf(char_63.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_62)
                Failure(p ~ ParseFailed("Expected one of \"", pos_62))
              }
          }
          catPart_59 <- {
            var parts_64 = ArrayBuffer.empty[PTree]
            var pos_65 = mark
            var res_66 = {
              val pos0_67 = mark
              val res_68 = for {
                catPart_69 <- {
                  val pos_71 = mark
                  val res_72 = {
                    val pos_73 = mark
                    expect('"')
                      .map { char_74 => PLeaf(char_74.toString) }
                      .recoverWith { case p: ParseError =>
                        reset(pos_73)
                        Failure(p ~ ParseFailed("Expected one of \"", pos_73))
                      }
                  }
                  reset(pos_71)
                  if (res_72.isSuccess) Failure(ParseFailed("Neglook failed", pos_71))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_70 <- Char()
              } yield PBranch("catPart_59", Seq(catPart_69, catPart_70))
              res_68.recoverWith { case p: ParseError =>
                reset(pos0_67)
                Failure(p)
              }
            }
            res_66.recover { _ => reset(pos_65) }
            while (res_66.isSuccess) {
              parts_64 += res_66.get
              pos_65 = mark
              res_66 = {
                val pos0_75 = mark
                val res_76 = for {
                  catPart_77 <- {
                    val pos_79 = mark
                    val res_80 = {
                      val pos_81 = mark
                      expect('"')
                        .map { char_82 => PLeaf(char_82.toString) }
                        .recoverWith { case p: ParseError =>
                          reset(pos_81)
                          Failure(p ~ ParseFailed("Expected one of \"", pos_81))
                        }
                    }
                    reset(pos_79)
                    if (res_80.isSuccess) Failure(ParseFailed("Neglook failed", pos_79))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_78 <- Char()
                } yield PBranch("catPart_59", Seq(catPart_77, catPart_78))
                res_76.recoverWith { case p: ParseError =>
                  reset(pos0_75)
                  Failure(p)
                }
              }
              res_66.recover { _ => reset(pos_65) }
            }
            Try(PBranch("catPart_59", parts_64.toSeq))
          }
          catPart_60 <- {
            val pos_83 = mark
            expect('"')
              .map { char_84 => PLeaf(char_84.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_83)
                Failure(p ~ ParseFailed("Expected one of \"", pos_83))
              }
          }
          catPart_61 <- Spacing()
        } yield PBranch("Literal", Seq(catPart_58, catPart_59, catPart_60, catPart_61))
        res_57.recoverWith { case p: ParseError =>
          reset(pos0_56)
          Failure(p)
        }
      }
      res_54.recoverWith { case err_55: ParseError =>
        reset(pos_22)
        val res_85 = {
          val pos0_87 = mark
          val res_88 = for {
            catPart_89 <- {
              val pos_93 = mark
              expect('`')
                .map { char_94 => PLeaf(char_94.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_93)
                  Failure(p ~ ParseFailed("Expected one of '`'", pos_93))
                }
            }
            catPart_90 <- {
              var parts_95 = ArrayBuffer.empty[PTree]
              var pos_96 = mark
              var res_97 = {
                val pos0_98 = mark
                val res_99 = for {
                  catPart_100 <- {
                    val pos_102 = mark
                    val res_103 = {
                      val pos_104 = mark
                      expect('`')
                        .map { char_105 => PLeaf(char_105.toString) }
                        .recoverWith { case p: ParseError =>
                          reset(pos_104)
                          Failure(p ~ ParseFailed("Expected one of '`'", pos_104))
                        }
                    }
                    reset(pos_102)
                    if (res_103.isSuccess) Failure(ParseFailed("Neglook failed", pos_102))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_101 <- Char()
                } yield PBranch("catPart_90", Seq(catPart_100, catPart_101))
                res_99.recoverWith { case p: ParseError =>
                  reset(pos0_98)
                  Failure(p)
                }
              }
              res_97.recover { _ => reset(pos_96) }
              while (res_97.isSuccess) {
                parts_95 += res_97.get
                pos_96 = mark
                res_97 = {
                  val pos0_106 = mark
                  val res_107 = for {
                    catPart_108 <- {
                      val pos_110 = mark
                      val res_111 = {
                        val pos_112 = mark
                        expect('`')
                          .map { char_113 => PLeaf(char_113.toString) }
                          .recoverWith { case p: ParseError =>
                            reset(pos_112)
                            Failure(p ~ ParseFailed("Expected one of '`'", pos_112))
                          }
                      }
                      reset(pos_110)
                      if (res_111.isSuccess) Failure(ParseFailed("Neglook failed", pos_110))
                      else {
                        Try(PEmpty)
                      }
                    }
                    catPart_109 <- Char()
                  } yield PBranch("catPart_90", Seq(catPart_108, catPart_109))
                  res_107.recoverWith { case p: ParseError =>
                    reset(pos0_106)
                    Failure(p)
                  }
                }
                res_97.recover { _ => reset(pos_96) }
              }
              Try(PBranch("catPart_90", parts_95.toSeq))
            }
            catPart_91 <- {
              val pos_114 = mark
              expect('`')
                .map { char_115 => PLeaf(char_115.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_114)
                  Failure(p ~ ParseFailed("Expected one of '`'", pos_114))
                }
            }
            catPart_92 <- Spacing()
          } yield PBranch("Literal", Seq(catPart_89, catPart_90, catPart_91, catPart_92))
          res_88.recoverWith { case p: ParseError =>
            reset(pos0_87)
            Failure(p)
          }
        }
        res_85.recoverWith { case err_86: ParseError =>
          reset(pos_22)
          Failure(err_24 ~ err_55 ~ err_86 ~ ParseFailed("", pos_22))
        }
      }
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_116 = mark
    val res_117 = for {
      catPart_118 <- {
        val pos_120 = mark
        val res_121 = for {
          char_part_122 <- expect(')')
        } yield PBranch("Lit", Seq(PLeaf(char_part_122.toString)))
        res_121.recoverWith { case p: ParseError =>
          reset(pos_120)
          Failure(p ~ ParseFailed("expected ')'", pos_120))
        }
      }
      catPart_119 <- Spacing()
    } yield PBranch("CLOSE", Seq(catPart_118, catPart_119))
    res_117.recoverWith { case p: ParseError =>
      reset(pos0_116)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_123 = mark
    val res_124 = for {
      catPart_125 <- Identifier()
      catPart_126 <- LEFTARROW()
      catPart_127 <- Expression()
    } yield PBranch("Definition", Seq(catPart_125, catPart_126, catPart_127))
    res_124.recoverWith { case p: ParseError =>
      reset(pos0_123)
      Failure(p)
    }
  }


  def Char(): Try[PTree] = {
    val pos_128 = mark
    val res_129 = {
      val pos0_131 = mark
      val res_132 = for {
        catPart_133 <- {
          val pos_135 = mark
          val res_136 = for {
            char_part_137 <- expect('\\')
          } yield PBranch("Lit", Seq(PLeaf(char_part_137.toString)))
          res_136.recoverWith { case p: ParseError =>
            reset(pos_135)
            Failure(p ~ ParseFailed("expected '\\'", pos_135))
          }
        }
        catPart_134 <- {
          val pos_138 = mark
          expect('n', '[', '\\', '\'', 'r', ']', 't', '"')
            .map { char_139 => PLeaf(char_139.toString) }
            .recoverWith { case p: ParseError =>
              reset(pos_138)
              Failure(p ~ ParseFailed("Expected one of 'n','[','\'','r',']','t','\\',\"", pos_138))
            }
        }
      } yield PBranch("Char", Seq(catPart_133, catPart_134))
      res_132.recoverWith { case p: ParseError =>
        reset(pos0_131)
        Failure(p)
      }
    }
    res_129.recoverWith { case err_130: ParseError =>
      reset(pos_128)
      val res_140 = {
        val pos0_142 = mark
        val res_143 = for {
          catPart_144 <- {
            val pos_146 = mark
            val res_147 = {
              val pos_148 = mark
              val res_149 = for {
                char_part_150 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_150.toString)))
              res_149.recoverWith { case p: ParseError =>
                reset(pos_148)
                Failure(p ~ ParseFailed("expected '\\'", pos_148))
              }
            }
            reset(pos_146)
            if (res_147.isSuccess) Failure(ParseFailed("Neglook failed", pos_146))
            else {
              Try(PEmpty)
            }
          }
          catPart_145 <- {
            val pos_151 = mark
            any.map { x => PLeaf(x.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_151)
                Failure(p ~ ParseFailed("Expected any char", pos_151))
              }
          }
        } yield PBranch("Char", Seq(catPart_144, catPart_145))
        res_143.recoverWith { case p: ParseError =>
          reset(pos0_142)
          Failure(p)
        }
      }
      res_140.recoverWith { case err_141: ParseError =>
        reset(pos_128)
        Failure(err_130 ~ err_141 ~ ParseFailed("", pos_128))
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_152 = mark
    val res_153 = for {
      catPart_154 <- {
        val pos_158 = mark
        val res_159 = for {
          char_part_160 <- expect('[')
        } yield PBranch("Lit", Seq(PLeaf(char_part_160.toString)))
        res_159.recoverWith { case p: ParseError =>
          reset(pos_158)
          Failure(p ~ ParseFailed("expected '['", pos_158))
        }
      }
      catPart_155 <- {
        var parts_161 = ArrayBuffer.empty[PTree]
        var pos_162 = mark
        var res_163 = {
          val pos0_164 = mark
          val res_165 = for {
            catPart_166 <- {
              val pos_168 = mark
              val res_169 = {
                val pos_170 = mark
                val res_171 = for {
                  char_part_172 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_172.toString)))
                res_171.recoverWith { case p: ParseError =>
                  reset(pos_170)
                  Failure(p ~ ParseFailed("expected ']'", pos_170))
                }
              }
              reset(pos_168)
              if (res_169.isSuccess) Failure(ParseFailed("Neglook failed", pos_168))
              else {
                Try(PEmpty)
              }
            }
            catPart_167 <- Range()
          } yield PBranch("catPart_155", Seq(catPart_166, catPart_167))
          res_165.recoverWith { case p: ParseError =>
            reset(pos0_164)
            Failure(p)
          }
        }
        res_163.recover { _ => reset(pos_162) }
        while (res_163.isSuccess) {
          parts_161 += res_163.get
          pos_162 = mark
          res_163 = {
            val pos0_173 = mark
            val res_174 = for {
              catPart_175 <- {
                val pos_177 = mark
                val res_178 = {
                  val pos_179 = mark
                  val res_180 = for {
                    char_part_181 <- expect(']')
                  } yield PBranch("Lit", Seq(PLeaf(char_part_181.toString)))
                  res_180.recoverWith { case p: ParseError =>
                    reset(pos_179)
                    Failure(p ~ ParseFailed("expected ']'", pos_179))
                  }
                }
                reset(pos_177)
                if (res_178.isSuccess) Failure(ParseFailed("Neglook failed", pos_177))
                else {
                  Try(PEmpty)
                }
              }
              catPart_176 <- Range()
            } yield PBranch("catPart_155", Seq(catPart_175, catPart_176))
            res_174.recoverWith { case p: ParseError =>
              reset(pos0_173)
              Failure(p)
            }
          }
          res_163.recover { _ => reset(pos_162) }
        }
        Try(PBranch("catPart_155", parts_161.toSeq))
      }
      catPart_156 <- {
        val pos_182 = mark
        val res_183 = for {
          char_part_184 <- expect(']')
        } yield PBranch("Lit", Seq(PLeaf(char_part_184.toString)))
        res_183.recoverWith { case p: ParseError =>
          reset(pos_182)
          Failure(p ~ ParseFailed("expected ']'", pos_182))
        }
      }
      catPart_157 <- Spacing()
    } yield PBranch("Class", Seq(catPart_154, catPart_155, catPart_156, catPart_157))
    res_153.recoverWith { case p: ParseError =>
      reset(pos0_152)
      Failure(p)
    }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_185 = mark
    val res_186 = {
      val pos_188 = mark
      val res_189 = for {
        char_part_190 <- expect('\r')
        char_part_191 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_190.toString), PLeaf(char_part_191.toString)))
      res_189.recoverWith { case p: ParseError =>
        reset(pos_188)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_188))
      }
    }
    res_186.recoverWith { case err_187: ParseError =>
      reset(pos_185)
      val res_192 = {
        val pos_194 = mark
        val res_195 = for {
          char_part_196 <- expect('\n')
        } yield PBranch("Lit", Seq(PLeaf(char_part_196.toString)))
        res_195.recoverWith { case p: ParseError =>
          reset(pos_194)
          Failure(p ~ ParseFailed("expected '\n'", pos_194))
        }
      }
      res_192.recoverWith { case err_193: ParseError =>
        reset(pos_185)
        val res_197 = {
          val pos_199 = mark
          val res_200 = for {
            char_part_201 <- expect('\r')
          } yield PBranch("Lit", Seq(PLeaf(char_part_201.toString)))
          res_200.recoverWith { case p: ParseError =>
            reset(pos_199)
            Failure(p ~ ParseFailed("expected '\r'", pos_199))
          }
        }
        res_197.recoverWith { case err_198: ParseError =>
          reset(pos_185)
          Failure(err_187 ~ err_193 ~ err_198 ~ ParseFailed("", pos_185))
        }
      }
    }
  }


  def IdentCont(): Try[PTree] = {
    val pos_202 = mark
    val res_203 = {
      val pos_205 = mark
      IdentStart().recoverWith { case p: ParseError =>
        reset(pos_205)
        Failure(p ~ ParseFailed("expected Var 'IdentStart'", pos_205))
      }
    }
    res_203.recoverWith { case err_204: ParseError =>
      reset(pos_202)
      val res_206 = {
        val pos_208 = mark
        expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
          .map { char_209 => PLeaf(char_209.toString) }
          .recoverWith { case p: ParseError =>
            reset(pos_208)
            Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_208))
          }
      }
      res_206.recoverWith { case err_207: ParseError =>
        reset(pos_202)
        Failure(err_204 ~ err_207 ~ ParseFailed("", pos_202))
      }
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_210 = mark
    val res_211 = for {
      catPart_212 <- {
        val pos_214 = mark
        val res_215 = for {
          char_part_216 <- expect('!')
        } yield PBranch("Lit", Seq(PLeaf(char_part_216.toString)))
        res_215.recoverWith { case p: ParseError =>
          reset(pos_214)
          Failure(p ~ ParseFailed("expected '!'", pos_214))
        }
      }
      catPart_213 <- Spacing()
    } yield PBranch("NOT", Seq(catPart_212, catPart_213))
    res_211.recoverWith { case p: ParseError =>
      reset(pos0_210)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_217 = mark
    val res_218 = for {
      catPart_219 <- {
        val pos_221 = mark
        val res_222 = for {
          char_part_223 <- expect('?')
        } yield PBranch("Lit", Seq(PLeaf(char_part_223.toString)))
        res_222.recoverWith { case p: ParseError =>
          reset(pos_221)
          Failure(p ~ ParseFailed("expected '?'", pos_221))
        }
      }
      catPart_220 <- Spacing()
    } yield PBranch("QUESTION", Seq(catPart_219, catPart_220))
    res_218.recoverWith { case p: ParseError =>
      reset(pos0_217)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_224 = mark
    val res_225 = for {
      catPart_226 <- Sequence()
      catPart_227 <- {
        var parts_228 = ArrayBuffer.empty[PTree]
        var pos_229 = mark
        var res_230 = {
          val pos0_231 = mark
          val res_232 = for {
            catPart_233 <- SLASH()
            catPart_234 <- Sequence()
          } yield PBranch("catPart_227", Seq(catPart_233, catPart_234))
          res_232.recoverWith { case p: ParseError =>
            reset(pos0_231)
            Failure(p)
          }
        }
        res_230.recover { _ => reset(pos_229) }
        while (res_230.isSuccess) {
          parts_228 += res_230.get
          pos_229 = mark
          res_230 = {
            val pos0_235 = mark
            val res_236 = for {
              catPart_237 <- SLASH()
              catPart_238 <- Sequence()
            } yield PBranch("catPart_227", Seq(catPart_237, catPart_238))
            res_236.recoverWith { case p: ParseError =>
              reset(pos0_235)
              Failure(p)
            }
          }
          res_230.recover { _ => reset(pos_229) }
        }
        Try(PBranch("catPart_227", parts_228.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_226, catPart_227))
    res_225.recoverWith { case p: ParseError =>
      reset(pos0_224)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    var parts_239 = ArrayBuffer.empty[PTree]
    var pos_240 = mark
    var res_241 = {
      val pos_242 = mark
      Prefix().recoverWith { case p: ParseError =>
        reset(pos_242)
        Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_242))
      }
    }
    res_241.recover { _ => reset(pos_240) }
    while (res_241.isSuccess) {
      parts_239 += res_241.get
      pos_240 = mark
      res_241 = {
        val pos_243 = mark
        Prefix().recoverWith { case p: ParseError =>
          reset(pos_243)
          Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_243))
        }
      }
      res_241.recover { _ => reset(pos_240) }
    }
    Try(PBranch("Sequence", parts_239.toSeq))
  }


  def Suffix(): Try[PTree] = {
    val pos0_244 = mark
    val res_245 = for {
      catPart_246 <- Primary()
      catPart_247 <- {
        val pos_248 = mark
        val res_249 = {
          val pos_251 = mark
          val res_252 = {
            val pos_254 = mark
            QUESTION().recoverWith { case p: ParseError =>
              reset(pos_254)
              Failure(p ~ ParseFailed("expected Var 'QUESTION'", pos_254))
            }
          }
          res_252.recoverWith { case err_253: ParseError =>
            reset(pos_251)
            val res_255 = {
              val pos_257 = mark
              STAR().recoverWith { case p: ParseError =>
                reset(pos_257)
                Failure(p ~ ParseFailed("expected Var 'STAR'", pos_257))
              }
            }
            res_255.recoverWith { case err_256: ParseError =>
              reset(pos_251)
              val res_258 = {
                val pos_260 = mark
                PLUS().recoverWith { case p: ParseError =>
                  reset(pos_260)
                  Failure(p ~ ParseFailed("expected Var 'PLUS'", pos_260))
                }
              }
              res_258.recoverWith { case err_259: ParseError =>
                reset(pos_251)
                Failure(err_253 ~ err_256 ~ err_259 ~ ParseFailed("", pos_251))
              }
            }
          }
        }
        res_249.recoverWith { case err_250: ParseError =>
          reset(pos_248)
          val res_261 = {
            Try(PEmpty)
          }
          res_261.recoverWith { case err_262: ParseError =>
            reset(pos_248)
            Failure(err_250 ~ err_262 ~ ParseFailed("", pos_248))
          }
        }
      }
    } yield PBranch("Suffix", Seq(catPart_246, catPart_247))
    res_245.recoverWith { case p: ParseError =>
      reset(pos0_244)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_263 = mark
    val res_264 = {
      val pos_266 = mark
      val res_267 = for {
        char_part_268 <- expect(' ')
      } yield PBranch("Lit", Seq(PLeaf(char_part_268.toString)))
      res_267.recoverWith { case p: ParseError =>
        reset(pos_266)
        Failure(p ~ ParseFailed("expected ' '", pos_266))
      }
    }
    res_264.recoverWith { case err_265: ParseError =>
      reset(pos_263)
      val res_269 = {
        val pos_271 = mark
        val res_272 = for {
          char_part_273 <- expect('\t')
        } yield PBranch("Lit", Seq(PLeaf(char_part_273.toString)))
        res_272.recoverWith { case p: ParseError =>
          reset(pos_271)
          Failure(p ~ ParseFailed("expected '\t'", pos_271))
        }
      }
      res_269.recoverWith { case err_270: ParseError =>
        reset(pos_263)
        val res_274 = {
          val pos_276 = mark
          EndOfLine().recoverWith { case p: ParseError =>
            reset(pos_276)
            Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_276))
          }
        }
        res_274.recoverWith { case err_275: ParseError =>
          reset(pos_263)
          Failure(err_265 ~ err_270 ~ err_275 ~ ParseFailed("", pos_263))
        }
      }
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_277 = mark
    val res_278 = for {
      catPart_279 <- {
        val pos_281 = mark
        val res_282 = for {
          char_part_283 <- expect('(')
        } yield PBranch("Lit", Seq(PLeaf(char_part_283.toString)))
        res_282.recoverWith { case p: ParseError =>
          reset(pos_281)
          Failure(p ~ ParseFailed("expected '('", pos_281))
        }
      }
      catPart_280 <- Spacing()
    } yield PBranch("OPEN", Seq(catPart_279, catPart_280))
    res_278.recoverWith { case p: ParseError =>
      reset(pos0_277)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_284 = mark
    val res_285 = {
      val pos_286 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_286)
          Failure(p ~ ParseFailed("Expected any char", pos_286))
        }
    }
    reset(pos_284)
    if (res_285.isSuccess) Failure(ParseFailed("Neglook failed", pos_284))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_287 = mark
    val res_288 = {
      val pos0_290 = mark
      val res_291 = for {
        catPart_292 <- Identifier()
        catPart_293 <- {
          val pos_294 = mark
          val res_295 = {
            val pos_296 = mark
            LEFTARROW().recoverWith { case p: ParseError =>
              reset(pos_296)
              Failure(p ~ ParseFailed("expected Var 'LEFTARROW'", pos_296))
            }
          }
          reset(pos_294)
          if (res_295.isSuccess) Failure(ParseFailed("Neglook failed", pos_294))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_292, catPart_293))
      res_291.recoverWith { case p: ParseError =>
        reset(pos0_290)
        Failure(p)
      }
    }
    res_288.recoverWith { case err_289: ParseError =>
      reset(pos_287)
      val res_297 = {
        val pos0_299 = mark
        val res_300 = for {
          catPart_301 <- OPEN()
          catPart_302 <- Expression()
          catPart_303 <- CLOSE()
        } yield PBranch("Primary", Seq(catPart_301, catPart_302, catPart_303))
        res_300.recoverWith { case p: ParseError =>
          reset(pos0_299)
          Failure(p)
        }
      }
      res_297.recoverWith { case err_298: ParseError =>
        reset(pos_287)
        val res_304 = {
          val pos_306 = mark
          Literal().recoverWith { case p: ParseError =>
            reset(pos_306)
            Failure(p ~ ParseFailed("expected Var 'Literal'", pos_306))
          }
        }
        res_304.recoverWith { case err_305: ParseError =>
          reset(pos_287)
          val res_307 = {
            val pos_309 = mark
            Class().recoverWith { case p: ParseError =>
              reset(pos_309)
              Failure(p ~ ParseFailed("expected Var 'Class'", pos_309))
            }
          }
          res_307.recoverWith { case err_308: ParseError =>
            reset(pos_287)
            val res_310 = {
              val pos_312 = mark
              DOT().recoverWith { case p: ParseError =>
                reset(pos_312)
                Failure(p ~ ParseFailed("expected Var 'DOT'", pos_312))
              }
            }
            res_310.recoverWith { case err_311: ParseError =>
              reset(pos_287)
              Failure(err_289 ~ err_298 ~ err_305 ~ err_308 ~ err_311 ~ ParseFailed("", pos_287))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    var parts_313 = ArrayBuffer.empty[PTree]
    var pos_314 = mark
    var res_315 = {
      val pos_316 = mark
      val res_317 = {
        val pos_319 = mark
        Space().recoverWith { case p: ParseError =>
          reset(pos_319)
          Failure(p ~ ParseFailed("expected Var 'Space'", pos_319))
        }
      }
      res_317.recoverWith { case err_318: ParseError =>
        reset(pos_316)
        val res_320 = {
          val pos_322 = mark
          Comment().recoverWith { case p: ParseError =>
            reset(pos_322)
            Failure(p ~ ParseFailed("expected Var 'Comment'", pos_322))
          }
        }
        res_320.recoverWith { case err_321: ParseError =>
          reset(pos_316)
          Failure(err_318 ~ err_321 ~ ParseFailed("", pos_316))
        }
      }
    }
    res_315.recover { _ => reset(pos_314) }
    while (res_315.isSuccess) {
      parts_313 += res_315.get
      pos_314 = mark
      res_315 = {
        val pos_323 = mark
        val res_324 = {
          val pos_326 = mark
          Space().recoverWith { case p: ParseError =>
            reset(pos_326)
            Failure(p ~ ParseFailed("expected Var 'Space'", pos_326))
          }
        }
        res_324.recoverWith { case err_325: ParseError =>
          reset(pos_323)
          val res_327 = {
            val pos_329 = mark
            Comment().recoverWith { case p: ParseError =>
              reset(pos_329)
              Failure(p ~ ParseFailed("expected Var 'Comment'", pos_329))
            }
          }
          res_327.recoverWith { case err_328: ParseError =>
            reset(pos_323)
            Failure(err_325 ~ err_328 ~ ParseFailed("", pos_323))
          }
        }
      }
      res_315.recover { _ => reset(pos_314) }
    }
    Try(PBranch("Spacing", parts_313.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_330 = mark
    expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
      .map { char_331 => PLeaf(char_331.toString) }
      .recoverWith { case p: ParseError =>
        reset(pos_330)
        Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_330))
      }
  }


  def Prefix(): Try[PTree] = {
    val pos0_332 = mark
    val res_333 = for {
      catPart_334 <- {
        val pos_336 = mark
        val res_337 = {
          val pos_339 = mark
          val res_340 = {
            val pos_342 = mark
            AND().recoverWith { case p: ParseError =>
              reset(pos_342)
              Failure(p ~ ParseFailed("expected Var 'AND'", pos_342))
            }
          }
          res_340.recoverWith { case err_341: ParseError =>
            reset(pos_339)
            val res_343 = {
              val pos_345 = mark
              NOT().recoverWith { case p: ParseError =>
                reset(pos_345)
                Failure(p ~ ParseFailed("expected Var 'NOT'", pos_345))
              }
            }
            res_343.recoverWith { case err_344: ParseError =>
              reset(pos_339)
              Failure(err_341 ~ err_344 ~ ParseFailed("", pos_339))
            }
          }
        }
        res_337.recoverWith { case err_338: ParseError =>
          reset(pos_336)
          val res_346 = {
            Try(PEmpty)
          }
          res_346.recoverWith { case err_347: ParseError =>
            reset(pos_336)
            Failure(err_338 ~ err_347 ~ ParseFailed("", pos_336))
          }
        }
      }
      catPart_335 <- Suffix()
    } yield PBranch("Prefix", Seq(catPart_334, catPart_335))
    res_333.recoverWith { case p: ParseError =>
      reset(pos0_332)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_348 = mark
    val res_349 = for {
      catPart_350 <- IdentStart()
      catPart_351 <- {
        var parts_353 = ArrayBuffer.empty[PTree]
        var pos_354 = mark
        var res_355 = {
          val pos_356 = mark
          IdentCont().recoverWith { case p: ParseError =>
            reset(pos_356)
            Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_356))
          }
        }
        res_355.recover { _ => reset(pos_354) }
        while (res_355.isSuccess) {
          parts_353 += res_355.get
          pos_354 = mark
          res_355 = {
            val pos_357 = mark
            IdentCont().recoverWith { case p: ParseError =>
              reset(pos_357)
              Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_357))
            }
          }
          res_355.recover { _ => reset(pos_354) }
        }
        Try(PBranch("catPart_351", parts_353.toSeq))
      }
      catPart_352 <- Spacing()
    } yield PBranch("Identifier", Seq(catPart_350, catPart_351, catPart_352))
    res_349.recoverWith { case p: ParseError =>
      reset(pos0_348)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_358 = mark
    val res_359 = for {
      catPart_360 <- {
        val pos_362 = mark
        val res_363 = for {
          char_part_364 <- expect('/')
        } yield PBranch("Lit", Seq(PLeaf(char_part_364.toString)))
        res_363.recoverWith { case p: ParseError =>
          reset(pos_362)
          Failure(p ~ ParseFailed("expected '/'", pos_362))
        }
      }
      catPart_361 <- Spacing()
    } yield PBranch("SLASH", Seq(catPart_360, catPart_361))
    res_359.recoverWith { case p: ParseError =>
      reset(pos0_358)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_365 = mark
    val res_366 = for {
      catPart_367 <- {
        val pos_369 = mark
        val res_370 = for {
          char_part_371 <- expect('<')
          char_part_372 <- expect('-')
        } yield PBranch("Lit", Seq(PLeaf(char_part_371.toString), PLeaf(char_part_372.toString)))
        res_370.recoverWith { case p: ParseError =>
          reset(pos_369)
          Failure(p ~ ParseFailed("expected '<','-'", pos_369))
        }
      }
      catPart_368 <- Spacing()
    } yield PBranch("LEFTARROW", Seq(catPart_367, catPart_368))
    res_366.recoverWith { case p: ParseError =>
      reset(pos0_365)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_373 = mark
    val res_374 = for {
      catPart_375 <- {
        val pos_378 = mark
        val res_379 = for {
          char_part_380 <- expect('#')
        } yield PBranch("Lit", Seq(PLeaf(char_part_380.toString)))
        res_379.recoverWith { case p: ParseError =>
          reset(pos_378)
          Failure(p ~ ParseFailed("expected '#'", pos_378))
        }
      }
      catPart_376 <- {
        var parts_381 = ArrayBuffer.empty[PTree]
        var pos_382 = mark
        var res_383 = {
          val pos0_384 = mark
          val res_385 = for {
            catPart_386 <- {
              val pos_388 = mark
              val res_389 = {
                val pos_390 = mark
                EndOfLine().recoverWith { case p: ParseError =>
                  reset(pos_390)
                  Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_390))
                }
              }
              reset(pos_388)
              if (res_389.isSuccess) Failure(ParseFailed("Neglook failed", pos_388))
              else {
                Try(PEmpty)
              }
            }
            catPart_387 <- {
              val pos_391 = mark
              any.map { x => PLeaf(x.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_391)
                  Failure(p ~ ParseFailed("Expected any char", pos_391))
                }
            }
          } yield PBranch("catPart_376", Seq(catPart_386, catPart_387))
          res_385.recoverWith { case p: ParseError =>
            reset(pos0_384)
            Failure(p)
          }
        }
        res_383.recover { _ => reset(pos_382) }
        while (res_383.isSuccess) {
          parts_381 += res_383.get
          pos_382 = mark
          res_383 = {
            val pos0_392 = mark
            val res_393 = for {
              catPart_394 <- {
                val pos_396 = mark
                val res_397 = {
                  val pos_398 = mark
                  EndOfLine().recoverWith { case p: ParseError =>
                    reset(pos_398)
                    Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_398))
                  }
                }
                reset(pos_396)
                if (res_397.isSuccess) Failure(ParseFailed("Neglook failed", pos_396))
                else {
                  Try(PEmpty)
                }
              }
              catPart_395 <- {
                val pos_399 = mark
                any.map { x => PLeaf(x.toString) }
                  .recoverWith { case p: ParseError =>
                    reset(pos_399)
                    Failure(p ~ ParseFailed("Expected any char", pos_399))
                  }
              }
            } yield PBranch("catPart_376", Seq(catPart_394, catPart_395))
            res_393.recoverWith { case p: ParseError =>
              reset(pos0_392)
              Failure(p)
            }
          }
          res_383.recover { _ => reset(pos_382) }
        }
        Try(PBranch("catPart_376", parts_381.toSeq))
      }
      catPart_377 <- EndOfLine()
    } yield PBranch("Comment", Seq(catPart_375, catPart_376, catPart_377))
    res_374.recoverWith { case p: ParseError =>
      reset(pos0_373)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_400 = mark
    val res_401 = for {
      catPart_402 <- {
        val pos_404 = mark
        val res_405 = for {
          char_part_406 <- expect('&')
        } yield PBranch("Lit", Seq(PLeaf(char_part_406.toString)))
        res_405.recoverWith { case p: ParseError =>
          reset(pos_404)
          Failure(p ~ ParseFailed("expected '&'", pos_404))
        }
      }
      catPart_403 <- Spacing()
    } yield PBranch("AND", Seq(catPart_402, catPart_403))
    res_401.recoverWith { case p: ParseError =>
      reset(pos0_400)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_407 = mark
    val res_408 = {
      val pos0_410 = mark
      val res_411 = for {
        catPart_412 <- Char()
        catPart_413 <- {
          val pos_415 = mark
          val res_416 = for {
            char_part_417 <- expect('-')
          } yield PBranch("Lit", Seq(PLeaf(char_part_417.toString)))
          res_416.recoverWith { case p: ParseError =>
            reset(pos_415)
            Failure(p ~ ParseFailed("expected '-'", pos_415))
          }
        }
        catPart_414 <- Char()
      } yield PBranch("Range", Seq(catPart_412, catPart_413, catPart_414))
      res_411.recoverWith { case p: ParseError =>
        reset(pos0_410)
        Failure(p)
      }
    }
    res_408.recoverWith { case err_409: ParseError =>
      reset(pos_407)
      val res_418 = {
        val pos_420 = mark
        Char().recoverWith { case p: ParseError =>
          reset(pos_420)
          Failure(p ~ ParseFailed("expected Var 'Char'", pos_420))
        }
      }
      res_418.recoverWith { case err_419: ParseError =>
        reset(pos_407)
        Failure(err_409 ~ err_419 ~ ParseFailed("", pos_407))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_421 = mark
    val res_422 = for {
      catPart_423 <- Spacing()
      catPart_424 <- {
        val pos0_426 = mark
        val res_427 = for {
          catPart_428 <- Definition()
          catPart_429 <- {
            var parts_430 = ArrayBuffer.empty[PTree]
            var pos_431 = mark
            var res_432 = {
              val pos_433 = mark
              Definition().recoverWith { case p: ParseError =>
                reset(pos_433)
                Failure(p ~ ParseFailed("expected Var 'Definition'", pos_433))
              }
            }
            res_432.recover { _ => reset(pos_431) }
            while (res_432.isSuccess) {
              parts_430 += res_432.get
              pos_431 = mark
              res_432 = {
                val pos_434 = mark
                Definition().recoverWith { case p: ParseError =>
                  reset(pos_434)
                  Failure(p ~ ParseFailed("expected Var 'Definition'", pos_434))
                }
              }
              res_432.recover { _ => reset(pos_431) }
            }
            Try(PBranch("catPart_429", parts_430.toSeq))
          }
        } yield PBranch("catPart_424", Seq(catPart_428, catPart_429))
        res_427.recoverWith { case p: ParseError =>
          reset(pos0_426)
          Failure(p)
        }
      }
      catPart_425 <- EndOfFile()
    } yield PBranch("Grammar", Seq(catPart_423, catPart_424, catPart_425))
    res_422.recoverWith { case p: ParseError =>
      reset(pos0_421)
      Failure(p)
    }
  }
}
