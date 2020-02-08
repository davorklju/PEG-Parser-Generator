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
      catPart_4 <- {
        val pos_8 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_8)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_8))
        }
      }
    } yield PBranch("DOT", Seq(catPart_3, catPart_4))
    res_2.recoverWith { case p: ParseError =>
      reset(pos0_1)
      Failure(p)
    }
  }


  def STAR(): Try[PTree] = {
    val pos0_9 = mark
    val res_10 = for {
      catPart_11 <- {
        val pos_13 = mark
        val res_14 = for {
          char_part_15 <- expect('*')
        } yield PBranch("Lit", Seq(PLeaf(char_part_15.toString)))
        res_14.recoverWith { case p: ParseError =>
          reset(pos_13)
          Failure(p ~ ParseFailed("expected '*'", pos_13))
        }
      }
      catPart_12 <- {
        val pos_16 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_16)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_16))
        }
      }
    } yield PBranch("STAR", Seq(catPart_11, catPart_12))
    res_10.recoverWith { case p: ParseError =>
      reset(pos0_9)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_17 = mark
    val res_18 = for {
      catPart_19 <- {
        val pos_21 = mark
        val res_22 = for {
          char_part_23 <- expect('+')
        } yield PBranch("Lit", Seq(PLeaf(char_part_23.toString)))
        res_22.recoverWith { case p: ParseError =>
          reset(pos_21)
          Failure(p ~ ParseFailed("expected '+'", pos_21))
        }
      }
      catPart_20 <- {
        val pos_24 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_24)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_24))
        }
      }
    } yield PBranch("PLUS", Seq(catPart_19, catPart_20))
    res_18.recoverWith { case p: ParseError =>
      reset(pos0_17)
      Failure(p)
    }
  }


  def Literal(): Try[PTree] = {
    val pos_25 = mark
    val res_26 = {
      val pos0_28 = mark
      val res_29 = for {
        catPart_30 <- {
          val pos_34 = mark
          val res_35 = for {
            char_36 <- expect('\'')
          } yield PLeaf(char_36.toString)
          res_35.recoverWith { case p: ParseError =>
            reset(pos_34)
            Failure(p ~ ParseFailed("Expected one of '\''", pos_34))
          }
        }
        catPart_31 <- {
          var parts_37 = ArrayBuffer.empty[PTree]
          var pos_38 = mark
          var res_39 = {
            val pos0_40 = mark
            val res_41 = for {
              catPart_42 <- {
                val pos_44 = mark
                val res_45 = {
                  val pos_46 = mark
                  val res_47 = for {
                    char_48 <- expect('\'')
                  } yield PLeaf(char_48.toString)
                  res_47.recoverWith { case p: ParseError =>
                    reset(pos_46)
                    Failure(p ~ ParseFailed("Expected one of '\''", pos_46))
                  }
                }
                reset(pos_44)
                if (res_45.isSuccess) Failure(ParseFailed("Neglook failed", pos_44))
                else {
                  Try(PEmpty)
                }
              }
              catPart_43 <- {
                val pos_49 = mark
                Char().recoverWith { case p: ParseError =>
                  reset(pos_49)
                  Failure(p ~ ParseFailed("expected Var 'Char'", pos_49))
                }
              }
            } yield PBranch("catPart_31", Seq(catPart_42, catPart_43))
            res_41.recoverWith { case p: ParseError =>
              reset(pos0_40)
              Failure(p)
            }
          }
          res_39.recover { _ => reset(pos_38) }
          while (res_39.isSuccess) {
            parts_37 += res_39.get
            pos_38 = mark
            res_39 = {
              val pos0_50 = mark
              val res_51 = for {
                catPart_52 <- {
                  val pos_54 = mark
                  val res_55 = {
                    val pos_56 = mark
                    val res_57 = for {
                      char_58 <- expect('\'')
                    } yield PLeaf(char_58.toString)
                    res_57.recoverWith { case p: ParseError =>
                      reset(pos_56)
                      Failure(p ~ ParseFailed("Expected one of '\''", pos_56))
                    }
                  }
                  reset(pos_54)
                  if (res_55.isSuccess) Failure(ParseFailed("Neglook failed", pos_54))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_53 <- {
                  val pos_59 = mark
                  Char().recoverWith { case p: ParseError =>
                    reset(pos_59)
                    Failure(p ~ ParseFailed("expected Var 'Char'", pos_59))
                  }
                }
              } yield PBranch("catPart_31", Seq(catPart_52, catPart_53))
              res_51.recoverWith { case p: ParseError =>
                reset(pos0_50)
                Failure(p)
              }
            }
            res_39.recover { _ => reset(pos_38) }
          }
          Try(PBranch("catPart_31", parts_37.toSeq))
        }
        catPart_32 <- {
          val pos_60 = mark
          val res_61 = for {
            char_62 <- expect('\'')
          } yield PLeaf(char_62.toString)
          res_61.recoverWith { case p: ParseError =>
            reset(pos_60)
            Failure(p ~ ParseFailed("Expected one of '\''", pos_60))
          }
        }
        catPart_33 <- {
          val pos_63 = mark
          Spacing().recoverWith { case p: ParseError =>
            reset(pos_63)
            Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_63))
          }
        }
      } yield PBranch("Literal", Seq(catPart_30, catPart_31, catPart_32, catPart_33))
      res_29.recoverWith { case p: ParseError =>
        reset(pos0_28)
        Failure(p)
      }
    }
    res_26.recoverWith { case err_27: ParseError =>
      reset(pos_25)
      val res_64 = {
        val pos0_66 = mark
        val res_67 = for {
          catPart_68 <- {
            val pos_72 = mark
            val res_73 = for {
              char_74 <- expect('"')
            } yield PLeaf(char_74.toString)
            res_73.recoverWith { case p: ParseError =>
              reset(pos_72)
              Failure(p ~ ParseFailed("Expected one of \"", pos_72))
            }
          }
          catPart_69 <- {
            var parts_75 = ArrayBuffer.empty[PTree]
            var pos_76 = mark
            var res_77 = {
              val pos0_78 = mark
              val res_79 = for {
                catPart_80 <- {
                  val pos_82 = mark
                  val res_83 = {
                    val pos_84 = mark
                    val res_85 = for {
                      char_86 <- expect('"')
                    } yield PLeaf(char_86.toString)
                    res_85.recoverWith { case p: ParseError =>
                      reset(pos_84)
                      Failure(p ~ ParseFailed("Expected one of \"", pos_84))
                    }
                  }
                  reset(pos_82)
                  if (res_83.isSuccess) Failure(ParseFailed("Neglook failed", pos_82))
                  else {
                    Try(PEmpty)
                  }
                }
                catPart_81 <- {
                  val pos_87 = mark
                  Char().recoverWith { case p: ParseError =>
                    reset(pos_87)
                    Failure(p ~ ParseFailed("expected Var 'Char'", pos_87))
                  }
                }
              } yield PBranch("catPart_69", Seq(catPart_80, catPart_81))
              res_79.recoverWith { case p: ParseError =>
                reset(pos0_78)
                Failure(p)
              }
            }
            res_77.recover { _ => reset(pos_76) }
            while (res_77.isSuccess) {
              parts_75 += res_77.get
              pos_76 = mark
              res_77 = {
                val pos0_88 = mark
                val res_89 = for {
                  catPart_90 <- {
                    val pos_92 = mark
                    val res_93 = {
                      val pos_94 = mark
                      val res_95 = for {
                        char_96 <- expect('"')
                      } yield PLeaf(char_96.toString)
                      res_95.recoverWith { case p: ParseError =>
                        reset(pos_94)
                        Failure(p ~ ParseFailed("Expected one of \"", pos_94))
                      }
                    }
                    reset(pos_92)
                    if (res_93.isSuccess) Failure(ParseFailed("Neglook failed", pos_92))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_91 <- {
                    val pos_97 = mark
                    Char().recoverWith { case p: ParseError =>
                      reset(pos_97)
                      Failure(p ~ ParseFailed("expected Var 'Char'", pos_97))
                    }
                  }
                } yield PBranch("catPart_69", Seq(catPart_90, catPart_91))
                res_89.recoverWith { case p: ParseError =>
                  reset(pos0_88)
                  Failure(p)
                }
              }
              res_77.recover { _ => reset(pos_76) }
            }
            Try(PBranch("catPart_69", parts_75.toSeq))
          }
          catPart_70 <- {
            val pos_98 = mark
            val res_99 = for {
              char_100 <- expect('"')
            } yield PLeaf(char_100.toString)
            res_99.recoverWith { case p: ParseError =>
              reset(pos_98)
              Failure(p ~ ParseFailed("Expected one of \"", pos_98))
            }
          }
          catPart_71 <- {
            val pos_101 = mark
            Spacing().recoverWith { case p: ParseError =>
              reset(pos_101)
              Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_101))
            }
          }
        } yield PBranch("Literal", Seq(catPart_68, catPart_69, catPart_70, catPart_71))
        res_67.recoverWith { case p: ParseError =>
          reset(pos0_66)
          Failure(p)
        }
      }
      res_64.recoverWith { case err_65: ParseError =>
        reset(pos_25)
        val res_102 = {
          val pos0_104 = mark
          val res_105 = for {
            catPart_106 <- {
              val pos_110 = mark
              val res_111 = for {
                char_112 <- expect('`')
              } yield PLeaf(char_112.toString)
              res_111.recoverWith { case p: ParseError =>
                reset(pos_110)
                Failure(p ~ ParseFailed("Expected one of '`'", pos_110))
              }
            }
            catPart_107 <- {
              var parts_113 = ArrayBuffer.empty[PTree]
              var pos_114 = mark
              var res_115 = {
                val pos0_116 = mark
                val res_117 = for {
                  catPart_118 <- {
                    val pos_120 = mark
                    val res_121 = {
                      val pos_122 = mark
                      val res_123 = for {
                        char_124 <- expect('`')
                      } yield PLeaf(char_124.toString)
                      res_123.recoverWith { case p: ParseError =>
                        reset(pos_122)
                        Failure(p ~ ParseFailed("Expected one of '`'", pos_122))
                      }
                    }
                    reset(pos_120)
                    if (res_121.isSuccess) Failure(ParseFailed("Neglook failed", pos_120))
                    else {
                      Try(PEmpty)
                    }
                  }
                  catPart_119 <- {
                    val pos_125 = mark
                    Char().recoverWith { case p: ParseError =>
                      reset(pos_125)
                      Failure(p ~ ParseFailed("expected Var 'Char'", pos_125))
                    }
                  }
                } yield PBranch("catPart_107", Seq(catPart_118, catPart_119))
                res_117.recoverWith { case p: ParseError =>
                  reset(pos0_116)
                  Failure(p)
                }
              }
              res_115.recover { _ => reset(pos_114) }
              while (res_115.isSuccess) {
                parts_113 += res_115.get
                pos_114 = mark
                res_115 = {
                  val pos0_126 = mark
                  val res_127 = for {
                    catPart_128 <- {
                      val pos_130 = mark
                      val res_131 = {
                        val pos_132 = mark
                        val res_133 = for {
                          char_134 <- expect('`')
                        } yield PLeaf(char_134.toString)
                        res_133.recoverWith { case p: ParseError =>
                          reset(pos_132)
                          Failure(p ~ ParseFailed("Expected one of '`'", pos_132))
                        }
                      }
                      reset(pos_130)
                      if (res_131.isSuccess) Failure(ParseFailed("Neglook failed", pos_130))
                      else {
                        Try(PEmpty)
                      }
                    }
                    catPart_129 <- {
                      val pos_135 = mark
                      Char().recoverWith { case p: ParseError =>
                        reset(pos_135)
                        Failure(p ~ ParseFailed("expected Var 'Char'", pos_135))
                      }
                    }
                  } yield PBranch("catPart_107", Seq(catPart_128, catPart_129))
                  res_127.recoverWith { case p: ParseError =>
                    reset(pos0_126)
                    Failure(p)
                  }
                }
                res_115.recover { _ => reset(pos_114) }
              }
              Try(PBranch("catPart_107", parts_113.toSeq))
            }
            catPart_108 <- {
              val pos_136 = mark
              val res_137 = for {
                char_138 <- expect('`')
              } yield PLeaf(char_138.toString)
              res_137.recoverWith { case p: ParseError =>
                reset(pos_136)
                Failure(p ~ ParseFailed("Expected one of '`'", pos_136))
              }
            }
            catPart_109 <- {
              val pos_139 = mark
              Spacing().recoverWith { case p: ParseError =>
                reset(pos_139)
                Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_139))
              }
            }
          } yield PBranch("Literal", Seq(catPart_106, catPart_107, catPart_108, catPart_109))
          res_105.recoverWith { case p: ParseError =>
            reset(pos0_104)
            Failure(p)
          }
        }
        res_102.recoverWith { case err_103: ParseError =>
          reset(pos_25)
          Failure(err_27 ~ err_65 ~ err_103 ~ ParseFailed("", pos_25))
        }
      }
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_140 = mark
    val res_141 = for {
      catPart_142 <- {
        val pos_144 = mark
        val res_145 = for {
          char_part_146 <- expect(')')
        } yield PBranch("Lit", Seq(PLeaf(char_part_146.toString)))
        res_145.recoverWith { case p: ParseError =>
          reset(pos_144)
          Failure(p ~ ParseFailed("expected ')'", pos_144))
        }
      }
      catPart_143 <- {
        val pos_147 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_147)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_147))
        }
      }
    } yield PBranch("CLOSE", Seq(catPart_142, catPart_143))
    res_141.recoverWith { case p: ParseError =>
      reset(pos0_140)
      Failure(p)
    }
  }


  def Definition(): Try[PTree] = {
    val pos0_148 = mark
    val res_149 = for {
      catPart_150 <- {
        val pos_153 = mark
        Identifier().recoverWith { case p: ParseError =>
          reset(pos_153)
          Failure(p ~ ParseFailed("expected Var 'Identifier'", pos_153))
        }
      }
      catPart_151 <- {
        val pos_154 = mark
        LEFTARROW().recoverWith { case p: ParseError =>
          reset(pos_154)
          Failure(p ~ ParseFailed("expected Var 'LEFTARROW'", pos_154))
        }
      }
      catPart_152 <- {
        val pos_155 = mark
        Expression().recoverWith { case p: ParseError =>
          reset(pos_155)
          Failure(p ~ ParseFailed("expected Var 'Expression'", pos_155))
        }
      }
    } yield PBranch("Definition", Seq(catPart_150, catPart_151, catPart_152))
    res_149.recoverWith { case p: ParseError =>
      reset(pos0_148)
      Failure(p)
    }
  }


  def Char(): Try[PTree] = {
    val pos_156 = mark
    val res_157 = {
      val pos0_159 = mark
      val res_160 = for {
        catPart_161 <- {
          val pos_163 = mark
          val res_164 = for {
            char_part_165 <- expect('\\')
          } yield PBranch("Lit", Seq(PLeaf(char_part_165.toString)))
          res_164.recoverWith { case p: ParseError =>
            reset(pos_163)
            Failure(p ~ ParseFailed("expected '\\'", pos_163))
          }
        }
        catPart_162 <- {
          val pos_166 = mark
          val res_167 = for {
            char_168 <- expect('n', '[', '\\', '\'', 'r', ']', 't', '"')
          } yield PLeaf(char_168.toString)
          res_167.recoverWith { case p: ParseError =>
            reset(pos_166)
            Failure(p ~ ParseFailed("Expected one of 'n','[','\'','r',']','t','\\',\"", pos_166))
          }
        }
      } yield PBranch("Char", Seq(catPart_161, catPart_162))
      res_160.recoverWith { case p: ParseError =>
        reset(pos0_159)
        Failure(p)
      }
    }
    res_157.recoverWith { case err_158: ParseError =>
      reset(pos_156)
      val res_169 = {
        val pos0_171 = mark
        val res_172 = for {
          catPart_173 <- {
            val pos_175 = mark
            val res_176 = {
              val pos_177 = mark
              val res_178 = for {
                char_part_179 <- expect('\\')
              } yield PBranch("Lit", Seq(PLeaf(char_part_179.toString)))
              res_178.recoverWith { case p: ParseError =>
                reset(pos_177)
                Failure(p ~ ParseFailed("expected '\\'", pos_177))
              }
            }
            reset(pos_175)
            if (res_176.isSuccess) Failure(ParseFailed("Neglook failed", pos_175))
            else {
              Try(PEmpty)
            }
          }
          catPart_174 <- {
            val pos_180 = mark
            any.map { x => PLeaf(x.toString) }
              .recoverWith { case p: ParseError =>
                reset(pos_180)
                Failure(p ~ ParseFailed("Expected any char", pos_180))
              }
          }
        } yield PBranch("Char", Seq(catPart_173, catPart_174))
        res_172.recoverWith { case p: ParseError =>
          reset(pos0_171)
          Failure(p)
        }
      }
      res_169.recoverWith { case err_170: ParseError =>
        reset(pos_156)
        Failure(err_158 ~ err_170 ~ ParseFailed("", pos_156))
      }
    }
  }


  def Class(): Try[PTree] = {
    val pos0_181 = mark
    val res_182 = for {
      catPart_183 <- {
        val pos_187 = mark
        val res_188 = for {
          char_part_189 <- expect('[')
        } yield PBranch("Lit", Seq(PLeaf(char_part_189.toString)))
        res_188.recoverWith { case p: ParseError =>
          reset(pos_187)
          Failure(p ~ ParseFailed("expected '['", pos_187))
        }
      }
      catPart_184 <- {
        var parts_190 = ArrayBuffer.empty[PTree]
        var pos_191 = mark
        var res_192 = {
          val pos0_193 = mark
          val res_194 = for {
            catPart_195 <- {
              val pos_197 = mark
              val res_198 = {
                val pos_199 = mark
                val res_200 = for {
                  char_part_201 <- expect(']')
                } yield PBranch("Lit", Seq(PLeaf(char_part_201.toString)))
                res_200.recoverWith { case p: ParseError =>
                  reset(pos_199)
                  Failure(p ~ ParseFailed("expected ']'", pos_199))
                }
              }
              reset(pos_197)
              if (res_198.isSuccess) Failure(ParseFailed("Neglook failed", pos_197))
              else {
                Try(PEmpty)
              }
            }
            catPart_196 <- {
              val pos_202 = mark
              Range().recoverWith { case p: ParseError =>
                reset(pos_202)
                Failure(p ~ ParseFailed("expected Var 'Range'", pos_202))
              }
            }
          } yield PBranch("catPart_184", Seq(catPart_195, catPart_196))
          res_194.recoverWith { case p: ParseError =>
            reset(pos0_193)
            Failure(p)
          }
        }
        res_192.recover { _ => reset(pos_191) }
        while (res_192.isSuccess) {
          parts_190 += res_192.get
          pos_191 = mark
          res_192 = {
            val pos0_203 = mark
            val res_204 = for {
              catPart_205 <- {
                val pos_207 = mark
                val res_208 = {
                  val pos_209 = mark
                  val res_210 = for {
                    char_part_211 <- expect(']')
                  } yield PBranch("Lit", Seq(PLeaf(char_part_211.toString)))
                  res_210.recoverWith { case p: ParseError =>
                    reset(pos_209)
                    Failure(p ~ ParseFailed("expected ']'", pos_209))
                  }
                }
                reset(pos_207)
                if (res_208.isSuccess) Failure(ParseFailed("Neglook failed", pos_207))
                else {
                  Try(PEmpty)
                }
              }
              catPart_206 <- {
                val pos_212 = mark
                Range().recoverWith { case p: ParseError =>
                  reset(pos_212)
                  Failure(p ~ ParseFailed("expected Var 'Range'", pos_212))
                }
              }
            } yield PBranch("catPart_184", Seq(catPart_205, catPart_206))
            res_204.recoverWith { case p: ParseError =>
              reset(pos0_203)
              Failure(p)
            }
          }
          res_192.recover { _ => reset(pos_191) }
        }
        Try(PBranch("catPart_184", parts_190.toSeq))
      }
      catPart_185 <- {
        val pos_213 = mark
        val res_214 = for {
          char_part_215 <- expect(']')
        } yield PBranch("Lit", Seq(PLeaf(char_part_215.toString)))
        res_214.recoverWith { case p: ParseError =>
          reset(pos_213)
          Failure(p ~ ParseFailed("expected ']'", pos_213))
        }
      }
      catPart_186 <- {
        val pos_216 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_216)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_216))
        }
      }
    } yield PBranch("Class", Seq(catPart_183, catPart_184, catPart_185, catPart_186))
    res_182.recoverWith { case p: ParseError =>
      reset(pos0_181)
      Failure(p)
    }
  }


  def EndOfLine(): Try[PTree] = {
    val pos_217 = mark
    val res_218 = {
      val pos_220 = mark
      val res_221 = for {
        char_part_222 <- expect('\r')
        char_part_223 <- expect('\n')
      } yield PBranch("Lit", Seq(PLeaf(char_part_222.toString), PLeaf(char_part_223.toString)))
      res_221.recoverWith { case p: ParseError =>
        reset(pos_220)
        Failure(p ~ ParseFailed("expected '\r','\n'", pos_220))
      }
    }
    res_218.recoverWith { case err_219: ParseError =>
      reset(pos_217)
      val res_224 = {
        val pos_226 = mark
        val res_227 = for {
          char_part_228 <- expect('\n')
        } yield PBranch("Lit", Seq(PLeaf(char_part_228.toString)))
        res_227.recoverWith { case p: ParseError =>
          reset(pos_226)
          Failure(p ~ ParseFailed("expected '\n'", pos_226))
        }
      }
      res_224.recoverWith { case err_225: ParseError =>
        reset(pos_217)
        val res_229 = {
          val pos_231 = mark
          val res_232 = for {
            char_part_233 <- expect('\r')
          } yield PBranch("Lit", Seq(PLeaf(char_part_233.toString)))
          res_232.recoverWith { case p: ParseError =>
            reset(pos_231)
            Failure(p ~ ParseFailed("expected '\r'", pos_231))
          }
        }
        res_229.recoverWith { case err_230: ParseError =>
          reset(pos_217)
          Failure(err_219 ~ err_225 ~ err_230 ~ ParseFailed("", pos_217))
        }
      }
    }
  }


  def IdentCont(): Try[PTree] = {
    val pos_234 = mark
    val res_235 = {
      val pos_237 = mark
      IdentStart().recoverWith { case p: ParseError =>
        reset(pos_237)
        Failure(p ~ ParseFailed("expected Var 'IdentStart'", pos_237))
      }
    }
    res_235.recoverWith { case err_236: ParseError =>
      reset(pos_234)
      val res_238 = {
        val pos_240 = mark
        val res_241 = for {
          char_242 <- expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
        } yield PLeaf(char_242.toString)
        res_241.recoverWith { case p: ParseError =>
          reset(pos_240)
          Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_240))
        }
      }
      res_238.recoverWith { case err_239: ParseError =>
        reset(pos_234)
        Failure(err_236 ~ err_239 ~ ParseFailed("", pos_234))
      }
    }
  }


  def NOT(): Try[PTree] = {
    val pos0_243 = mark
    val res_244 = for {
      catPart_245 <- {
        val pos_247 = mark
        val res_248 = for {
          char_part_249 <- expect('!')
        } yield PBranch("Lit", Seq(PLeaf(char_part_249.toString)))
        res_248.recoverWith { case p: ParseError =>
          reset(pos_247)
          Failure(p ~ ParseFailed("expected '!'", pos_247))
        }
      }
      catPart_246 <- {
        val pos_250 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_250)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_250))
        }
      }
    } yield PBranch("NOT", Seq(catPart_245, catPart_246))
    res_244.recoverWith { case p: ParseError =>
      reset(pos0_243)
      Failure(p)
    }
  }


  def QUESTION(): Try[PTree] = {
    val pos0_251 = mark
    val res_252 = for {
      catPart_253 <- {
        val pos_255 = mark
        val res_256 = for {
          char_part_257 <- expect('?')
        } yield PBranch("Lit", Seq(PLeaf(char_part_257.toString)))
        res_256.recoverWith { case p: ParseError =>
          reset(pos_255)
          Failure(p ~ ParseFailed("expected '?'", pos_255))
        }
      }
      catPart_254 <- {
        val pos_258 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_258)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_258))
        }
      }
    } yield PBranch("QUESTION", Seq(catPart_253, catPart_254))
    res_252.recoverWith { case p: ParseError =>
      reset(pos0_251)
      Failure(p)
    }
  }


  def Expression(): Try[PTree] = {
    val pos0_259 = mark
    val res_260 = for {
      catPart_261 <- {
        val pos_263 = mark
        Sequence().recoverWith { case p: ParseError =>
          reset(pos_263)
          Failure(p ~ ParseFailed("expected Var 'Sequence'", pos_263))
        }
      }
      catPart_262 <- {
        var parts_264 = ArrayBuffer.empty[PTree]
        var pos_265 = mark
        var res_266 = {
          val pos0_267 = mark
          val res_268 = for {
            catPart_269 <- {
              val pos_271 = mark
              SLASH().recoverWith { case p: ParseError =>
                reset(pos_271)
                Failure(p ~ ParseFailed("expected Var 'SLASH'", pos_271))
              }
            }
            catPart_270 <- {
              val pos_272 = mark
              Sequence().recoverWith { case p: ParseError =>
                reset(pos_272)
                Failure(p ~ ParseFailed("expected Var 'Sequence'", pos_272))
              }
            }
          } yield PBranch("catPart_262", Seq(catPart_269, catPart_270))
          res_268.recoverWith { case p: ParseError =>
            reset(pos0_267)
            Failure(p)
          }
        }
        res_266.recover { _ => reset(pos_265) }
        while (res_266.isSuccess) {
          parts_264 += res_266.get
          pos_265 = mark
          res_266 = {
            val pos0_273 = mark
            val res_274 = for {
              catPart_275 <- {
                val pos_277 = mark
                SLASH().recoverWith { case p: ParseError =>
                  reset(pos_277)
                  Failure(p ~ ParseFailed("expected Var 'SLASH'", pos_277))
                }
              }
              catPart_276 <- {
                val pos_278 = mark
                Sequence().recoverWith { case p: ParseError =>
                  reset(pos_278)
                  Failure(p ~ ParseFailed("expected Var 'Sequence'", pos_278))
                }
              }
            } yield PBranch("catPart_262", Seq(catPart_275, catPart_276))
            res_274.recoverWith { case p: ParseError =>
              reset(pos0_273)
              Failure(p)
            }
          }
          res_266.recover { _ => reset(pos_265) }
        }
        Try(PBranch("catPart_262", parts_264.toSeq))
      }
    } yield PBranch("Expression", Seq(catPart_261, catPart_262))
    res_260.recoverWith { case p: ParseError =>
      reset(pos0_259)
      Failure(p)
    }
  }


  def Sequence(): Try[PTree] = {
    var parts_279 = ArrayBuffer.empty[PTree]
    var pos_280 = mark
    var res_281 = {
      val pos_282 = mark
      Prefix().recoverWith { case p: ParseError =>
        reset(pos_282)
        Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_282))
      }
    }
    res_281.recover { _ => reset(pos_280) }
    while (res_281.isSuccess) {
      parts_279 += res_281.get
      pos_280 = mark
      res_281 = {
        val pos_283 = mark
        Prefix().recoverWith { case p: ParseError =>
          reset(pos_283)
          Failure(p ~ ParseFailed("expected Var 'Prefix'", pos_283))
        }
      }
      res_281.recover { _ => reset(pos_280) }
    }
    Try(PBranch("Sequence", parts_279.toSeq))
  }


  def Suffix(): Try[PTree] = {
    val pos0_284 = mark
    val res_285 = for {
      catPart_286 <- {
        val pos_288 = mark
        Primary().recoverWith { case p: ParseError =>
          reset(pos_288)
          Failure(p ~ ParseFailed("expected Var 'Primary'", pos_288))
        }
      }
      catPart_287 <- {
        val pos_289 = mark
        val res_290 = {
          val pos_292 = mark
          val res_293 = {
            val pos_295 = mark
            QUESTION().recoverWith { case p: ParseError =>
              reset(pos_295)
              Failure(p ~ ParseFailed("expected Var 'QUESTION'", pos_295))
            }
          }
          res_293.recoverWith { case err_294: ParseError =>
            reset(pos_292)
            val res_296 = {
              val pos_298 = mark
              STAR().recoverWith { case p: ParseError =>
                reset(pos_298)
                Failure(p ~ ParseFailed("expected Var 'STAR'", pos_298))
              }
            }
            res_296.recoverWith { case err_297: ParseError =>
              reset(pos_292)
              val res_299 = {
                val pos_301 = mark
                PLUS().recoverWith { case p: ParseError =>
                  reset(pos_301)
                  Failure(p ~ ParseFailed("expected Var 'PLUS'", pos_301))
                }
              }
              res_299.recoverWith { case err_300: ParseError =>
                reset(pos_292)
                Failure(err_294 ~ err_297 ~ err_300 ~ ParseFailed("", pos_292))
              }
            }
          }
        }
        res_290.recoverWith { case err_291: ParseError =>
          reset(pos_289)
          val res_302 = {
            Try(PEmpty)
          }
          res_302.recoverWith { case err_303: ParseError =>
            reset(pos_289)
            Failure(err_291 ~ err_303 ~ ParseFailed("", pos_289))
          }
        }
      }
    } yield PBranch("Suffix", Seq(catPart_286, catPart_287))
    res_285.recoverWith { case p: ParseError =>
      reset(pos0_284)
      Failure(p)
    }
  }


  def Space(): Try[PTree] = {
    val pos_304 = mark
    val res_305 = {
      val pos_307 = mark
      val res_308 = for {
        char_part_309 <- expect(' ')
      } yield PBranch("Lit", Seq(PLeaf(char_part_309.toString)))
      res_308.recoverWith { case p: ParseError =>
        reset(pos_307)
        Failure(p ~ ParseFailed("expected ' '", pos_307))
      }
    }
    res_305.recoverWith { case err_306: ParseError =>
      reset(pos_304)
      val res_310 = {
        val pos_312 = mark
        val res_313 = for {
          char_part_314 <- expect('\t')
        } yield PBranch("Lit", Seq(PLeaf(char_part_314.toString)))
        res_313.recoverWith { case p: ParseError =>
          reset(pos_312)
          Failure(p ~ ParseFailed("expected '\t'", pos_312))
        }
      }
      res_310.recoverWith { case err_311: ParseError =>
        reset(pos_304)
        val res_315 = {
          val pos_317 = mark
          EndOfLine().recoverWith { case p: ParseError =>
            reset(pos_317)
            Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_317))
          }
        }
        res_315.recoverWith { case err_316: ParseError =>
          reset(pos_304)
          Failure(err_306 ~ err_311 ~ err_316 ~ ParseFailed("", pos_304))
        }
      }
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_318 = mark
    val res_319 = for {
      catPart_320 <- {
        val pos_322 = mark
        val res_323 = for {
          char_part_324 <- expect('(')
        } yield PBranch("Lit", Seq(PLeaf(char_part_324.toString)))
        res_323.recoverWith { case p: ParseError =>
          reset(pos_322)
          Failure(p ~ ParseFailed("expected '('", pos_322))
        }
      }
      catPart_321 <- {
        val pos_325 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_325)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_325))
        }
      }
    } yield PBranch("OPEN", Seq(catPart_320, catPart_321))
    res_319.recoverWith { case p: ParseError =>
      reset(pos0_318)
      Failure(p)
    }
  }


  def EndOfFile(): Try[PTree] = {
    val pos_326 = mark
    val res_327 = {
      val pos_328 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError =>
          reset(pos_328)
          Failure(p ~ ParseFailed("Expected any char", pos_328))
        }
    }
    reset(pos_326)
    if (res_327.isSuccess) Failure(ParseFailed("Neglook failed", pos_326))
    else {
      Try(PEmpty)
    }
  }


  def Primary(): Try[PTree] = {
    val pos_329 = mark
    val res_330 = {
      val pos0_332 = mark
      val res_333 = for {
        catPart_334 <- {
          val pos_336 = mark
          Identifier().recoverWith { case p: ParseError =>
            reset(pos_336)
            Failure(p ~ ParseFailed("expected Var 'Identifier'", pos_336))
          }
        }
        catPart_335 <- {
          val pos_337 = mark
          val res_338 = {
            val pos_339 = mark
            LEFTARROW().recoverWith { case p: ParseError =>
              reset(pos_339)
              Failure(p ~ ParseFailed("expected Var 'LEFTARROW'", pos_339))
            }
          }
          reset(pos_337)
          if (res_338.isSuccess) Failure(ParseFailed("Neglook failed", pos_337))
          else {
            Try(PEmpty)
          }
        }
      } yield PBranch("Primary", Seq(catPart_334, catPart_335))
      res_333.recoverWith { case p: ParseError =>
        reset(pos0_332)
        Failure(p)
      }
    }
    res_330.recoverWith { case err_331: ParseError =>
      reset(pos_329)
      val res_340 = {
        val pos0_342 = mark
        val res_343 = for {
          catPart_344 <- {
            val pos_347 = mark
            OPEN().recoverWith { case p: ParseError =>
              reset(pos_347)
              Failure(p ~ ParseFailed("expected Var 'OPEN'", pos_347))
            }
          }
          catPart_345 <- {
            val pos_348 = mark
            Expression().recoverWith { case p: ParseError =>
              reset(pos_348)
              Failure(p ~ ParseFailed("expected Var 'Expression'", pos_348))
            }
          }
          catPart_346 <- {
            val pos_349 = mark
            CLOSE().recoverWith { case p: ParseError =>
              reset(pos_349)
              Failure(p ~ ParseFailed("expected Var 'CLOSE'", pos_349))
            }
          }
        } yield PBranch("Primary", Seq(catPart_344, catPart_345, catPart_346))
        res_343.recoverWith { case p: ParseError =>
          reset(pos0_342)
          Failure(p)
        }
      }
      res_340.recoverWith { case err_341: ParseError =>
        reset(pos_329)
        val res_350 = {
          val pos_352 = mark
          Literal().recoverWith { case p: ParseError =>
            reset(pos_352)
            Failure(p ~ ParseFailed("expected Var 'Literal'", pos_352))
          }
        }
        res_350.recoverWith { case err_351: ParseError =>
          reset(pos_329)
          val res_353 = {
            val pos_355 = mark
            Class().recoverWith { case p: ParseError =>
              reset(pos_355)
              Failure(p ~ ParseFailed("expected Var 'Class'", pos_355))
            }
          }
          res_353.recoverWith { case err_354: ParseError =>
            reset(pos_329)
            val res_356 = {
              val pos_358 = mark
              DOT().recoverWith { case p: ParseError =>
                reset(pos_358)
                Failure(p ~ ParseFailed("expected Var 'DOT'", pos_358))
              }
            }
            res_356.recoverWith { case err_357: ParseError =>
              reset(pos_329)
              Failure(err_331 ~ err_341 ~ err_351 ~ err_354 ~ err_357 ~ ParseFailed("", pos_329))
            }
          }
        }
      }
    }
  }


  def Spacing(): Try[PTree] = {
    var parts_359 = ArrayBuffer.empty[PTree]
    var pos_360 = mark
    var res_361 = {
      val pos_362 = mark
      val res_363 = {
        val pos_365 = mark
        Space().recoverWith { case p: ParseError =>
          reset(pos_365)
          Failure(p ~ ParseFailed("expected Var 'Space'", pos_365))
        }
      }
      res_363.recoverWith { case err_364: ParseError =>
        reset(pos_362)
        val res_366 = {
          val pos_368 = mark
          Comment().recoverWith { case p: ParseError =>
            reset(pos_368)
            Failure(p ~ ParseFailed("expected Var 'Comment'", pos_368))
          }
        }
        res_366.recoverWith { case err_367: ParseError =>
          reset(pos_362)
          Failure(err_364 ~ err_367 ~ ParseFailed("", pos_362))
        }
      }
    }
    res_361.recover { _ => reset(pos_360) }
    while (res_361.isSuccess) {
      parts_359 += res_361.get
      pos_360 = mark
      res_361 = {
        val pos_369 = mark
        val res_370 = {
          val pos_372 = mark
          Space().recoverWith { case p: ParseError =>
            reset(pos_372)
            Failure(p ~ ParseFailed("expected Var 'Space'", pos_372))
          }
        }
        res_370.recoverWith { case err_371: ParseError =>
          reset(pos_369)
          val res_373 = {
            val pos_375 = mark
            Comment().recoverWith { case p: ParseError =>
              reset(pos_375)
              Failure(p ~ ParseFailed("expected Var 'Comment'", pos_375))
            }
          }
          res_373.recoverWith { case err_374: ParseError =>
            reset(pos_369)
            Failure(err_371 ~ err_374 ~ ParseFailed("", pos_369))
          }
        }
      }
      res_361.recover { _ => reset(pos_360) }
    }
    Try(PBranch("Spacing", parts_359.toSeq))
  }


  def IdentStart(): Try[PTree] = {
    val pos_376 = mark
    val res_377 = for {
      char_378 <- expect('v', 'x', 'H', '_', 'e', 'I', 'M', 'b', 'R', 'n', 'a', 'F', 'D', 'u', 'y', 'X', 'k', 'h', 'V', 'E', 'A', 'f', 'J', 'S', 'W', 'z', 'r', 'g', 'l', 'c', 'P', 'q', 'Y', 'm', 'O', 'B', 's', 'K', 'd', 'T', 'N', 'Q', 'U', 'Z', 'i', 'j', 'p', 'C', 'G', 'L', 't', 'w', 'o')
    } yield PLeaf(char_378.toString)
    res_377.recoverWith { case p: ParseError =>
      reset(pos_376)
      Failure(p ~ ParseFailed("Expected one of 'v','x','H','_','e','I','M','b','R','n','a','F','D','u','y','X','k','h','V','E','A','f','J','S','W','z','r','g','l','c','P','q','Y','m','O','B','s','K','d','T','N','Q','U','Z','i','j','p','C','G','L','t','w','o'", pos_376))
    }
  }


  def Prefix(): Try[PTree] = {
    val pos0_379 = mark
    val res_380 = for {
      catPart_381 <- {
        val pos_383 = mark
        val res_384 = {
          val pos_386 = mark
          val res_387 = {
            val pos_389 = mark
            AND().recoverWith { case p: ParseError =>
              reset(pos_389)
              Failure(p ~ ParseFailed("expected Var 'AND'", pos_389))
            }
          }
          res_387.recoverWith { case err_388: ParseError =>
            reset(pos_386)
            val res_390 = {
              val pos_392 = mark
              NOT().recoverWith { case p: ParseError =>
                reset(pos_392)
                Failure(p ~ ParseFailed("expected Var 'NOT'", pos_392))
              }
            }
            res_390.recoverWith { case err_391: ParseError =>
              reset(pos_386)
              Failure(err_388 ~ err_391 ~ ParseFailed("", pos_386))
            }
          }
        }
        res_384.recoverWith { case err_385: ParseError =>
          reset(pos_383)
          val res_393 = {
            Try(PEmpty)
          }
          res_393.recoverWith { case err_394: ParseError =>
            reset(pos_383)
            Failure(err_385 ~ err_394 ~ ParseFailed("", pos_383))
          }
        }
      }
      catPart_382 <- {
        val pos_395 = mark
        Suffix().recoverWith { case p: ParseError =>
          reset(pos_395)
          Failure(p ~ ParseFailed("expected Var 'Suffix'", pos_395))
        }
      }
    } yield PBranch("Prefix", Seq(catPart_381, catPart_382))
    res_380.recoverWith { case p: ParseError =>
      reset(pos0_379)
      Failure(p)
    }
  }


  def Identifier(): Try[PTree] = {
    val pos0_396 = mark
    val res_397 = for {
      catPart_398 <- {
        val pos_401 = mark
        IdentStart().recoverWith { case p: ParseError =>
          reset(pos_401)
          Failure(p ~ ParseFailed("expected Var 'IdentStart'", pos_401))
        }
      }
      catPart_399 <- {
        var parts_402 = ArrayBuffer.empty[PTree]
        var pos_403 = mark
        var res_404 = {
          val pos_405 = mark
          IdentCont().recoverWith { case p: ParseError =>
            reset(pos_405)
            Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_405))
          }
        }
        res_404.recover { _ => reset(pos_403) }
        while (res_404.isSuccess) {
          parts_402 += res_404.get
          pos_403 = mark
          res_404 = {
            val pos_406 = mark
            IdentCont().recoverWith { case p: ParseError =>
              reset(pos_406)
              Failure(p ~ ParseFailed("expected Var 'IdentCont'", pos_406))
            }
          }
          res_404.recover { _ => reset(pos_403) }
        }
        Try(PBranch("catPart_399", parts_402.toSeq))
      }
      catPart_400 <- {
        val pos_407 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_407)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_407))
        }
      }
    } yield PBranch("Identifier", Seq(catPart_398, catPart_399, catPart_400))
    res_397.recoverWith { case p: ParseError =>
      reset(pos0_396)
      Failure(p)
    }
  }


  def SLASH(): Try[PTree] = {
    val pos0_408 = mark
    val res_409 = for {
      catPart_410 <- {
        val pos_412 = mark
        val res_413 = for {
          char_part_414 <- expect('/')
        } yield PBranch("Lit", Seq(PLeaf(char_part_414.toString)))
        res_413.recoverWith { case p: ParseError =>
          reset(pos_412)
          Failure(p ~ ParseFailed("expected '/'", pos_412))
        }
      }
      catPart_411 <- {
        val pos_415 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_415)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_415))
        }
      }
    } yield PBranch("SLASH", Seq(catPart_410, catPart_411))
    res_409.recoverWith { case p: ParseError =>
      reset(pos0_408)
      Failure(p)
    }
  }


  def LEFTARROW(): Try[PTree] = {
    val pos0_416 = mark
    val res_417 = for {
      catPart_418 <- {
        val pos_420 = mark
        val res_421 = for {
          char_part_422 <- expect('<')
          char_part_423 <- expect('-')
        } yield PBranch("Lit", Seq(PLeaf(char_part_422.toString), PLeaf(char_part_423.toString)))
        res_421.recoverWith { case p: ParseError =>
          reset(pos_420)
          Failure(p ~ ParseFailed("expected '<','-'", pos_420))
        }
      }
      catPart_419 <- {
        val pos_424 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_424)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_424))
        }
      }
    } yield PBranch("LEFTARROW", Seq(catPart_418, catPart_419))
    res_417.recoverWith { case p: ParseError =>
      reset(pos0_416)
      Failure(p)
    }
  }


  def Comment(): Try[PTree] = {
    val pos0_425 = mark
    val res_426 = for {
      catPart_427 <- {
        val pos_430 = mark
        val res_431 = for {
          char_part_432 <- expect('#')
        } yield PBranch("Lit", Seq(PLeaf(char_part_432.toString)))
        res_431.recoverWith { case p: ParseError =>
          reset(pos_430)
          Failure(p ~ ParseFailed("expected '#'", pos_430))
        }
      }
      catPart_428 <- {
        var parts_433 = ArrayBuffer.empty[PTree]
        var pos_434 = mark
        var res_435 = {
          val pos0_436 = mark
          val res_437 = for {
            catPart_438 <- {
              val pos_440 = mark
              val res_441 = {
                val pos_442 = mark
                EndOfLine().recoverWith { case p: ParseError =>
                  reset(pos_442)
                  Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_442))
                }
              }
              reset(pos_440)
              if (res_441.isSuccess) Failure(ParseFailed("Neglook failed", pos_440))
              else {
                Try(PEmpty)
              }
            }
            catPart_439 <- {
              val pos_443 = mark
              any.map { x => PLeaf(x.toString) }
                .recoverWith { case p: ParseError =>
                  reset(pos_443)
                  Failure(p ~ ParseFailed("Expected any char", pos_443))
                }
            }
          } yield PBranch("catPart_428", Seq(catPart_438, catPart_439))
          res_437.recoverWith { case p: ParseError =>
            reset(pos0_436)
            Failure(p)
          }
        }
        res_435.recover { _ => reset(pos_434) }
        while (res_435.isSuccess) {
          parts_433 += res_435.get
          pos_434 = mark
          res_435 = {
            val pos0_444 = mark
            val res_445 = for {
              catPart_446 <- {
                val pos_448 = mark
                val res_449 = {
                  val pos_450 = mark
                  EndOfLine().recoverWith { case p: ParseError =>
                    reset(pos_450)
                    Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_450))
                  }
                }
                reset(pos_448)
                if (res_449.isSuccess) Failure(ParseFailed("Neglook failed", pos_448))
                else {
                  Try(PEmpty)
                }
              }
              catPart_447 <- {
                val pos_451 = mark
                any.map { x => PLeaf(x.toString) }
                  .recoverWith { case p: ParseError =>
                    reset(pos_451)
                    Failure(p ~ ParseFailed("Expected any char", pos_451))
                  }
              }
            } yield PBranch("catPart_428", Seq(catPart_446, catPart_447))
            res_445.recoverWith { case p: ParseError =>
              reset(pos0_444)
              Failure(p)
            }
          }
          res_435.recover { _ => reset(pos_434) }
        }
        Try(PBranch("catPart_428", parts_433.toSeq))
      }
      catPart_429 <- {
        val pos_452 = mark
        EndOfLine().recoverWith { case p: ParseError =>
          reset(pos_452)
          Failure(p ~ ParseFailed("expected Var 'EndOfLine'", pos_452))
        }
      }
    } yield PBranch("Comment", Seq(catPart_427, catPart_428, catPart_429))
    res_426.recoverWith { case p: ParseError =>
      reset(pos0_425)
      Failure(p)
    }
  }


  def AND(): Try[PTree] = {
    val pos0_453 = mark
    val res_454 = for {
      catPart_455 <- {
        val pos_457 = mark
        val res_458 = for {
          char_part_459 <- expect('&')
        } yield PBranch("Lit", Seq(PLeaf(char_part_459.toString)))
        res_458.recoverWith { case p: ParseError =>
          reset(pos_457)
          Failure(p ~ ParseFailed("expected '&'", pos_457))
        }
      }
      catPart_456 <- {
        val pos_460 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_460)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_460))
        }
      }
    } yield PBranch("AND", Seq(catPart_455, catPart_456))
    res_454.recoverWith { case p: ParseError =>
      reset(pos0_453)
      Failure(p)
    }
  }


  def Range(): Try[PTree] = {
    val pos_461 = mark
    val res_462 = {
      val pos0_464 = mark
      val res_465 = for {
        catPart_466 <- {
          val pos_469 = mark
          Char().recoverWith { case p: ParseError =>
            reset(pos_469)
            Failure(p ~ ParseFailed("expected Var 'Char'", pos_469))
          }
        }
        catPart_467 <- {
          val pos_470 = mark
          val res_471 = for {
            char_part_472 <- expect('-')
          } yield PBranch("Lit", Seq(PLeaf(char_part_472.toString)))
          res_471.recoverWith { case p: ParseError =>
            reset(pos_470)
            Failure(p ~ ParseFailed("expected '-'", pos_470))
          }
        }
        catPart_468 <- {
          val pos_473 = mark
          Char().recoverWith { case p: ParseError =>
            reset(pos_473)
            Failure(p ~ ParseFailed("expected Var 'Char'", pos_473))
          }
        }
      } yield PBranch("Range", Seq(catPart_466, catPart_467, catPart_468))
      res_465.recoverWith { case p: ParseError =>
        reset(pos0_464)
        Failure(p)
      }
    }
    res_462.recoverWith { case err_463: ParseError =>
      reset(pos_461)
      val res_474 = {
        val pos_476 = mark
        Char().recoverWith { case p: ParseError =>
          reset(pos_476)
          Failure(p ~ ParseFailed("expected Var 'Char'", pos_476))
        }
      }
      res_474.recoverWith { case err_475: ParseError =>
        reset(pos_461)
        Failure(err_463 ~ err_475 ~ ParseFailed("", pos_461))
      }
    }
  }


  def Grammar(): Try[PTree] = {
    val pos0_477 = mark
    val res_478 = for {
      catPart_479 <- {
        val pos_482 = mark
        Spacing().recoverWith { case p: ParseError =>
          reset(pos_482)
          Failure(p ~ ParseFailed("expected Var 'Spacing'", pos_482))
        }
      }
      catPart_480 <- {
        val pos0_483 = mark
        val res_484 = for {
          catPart_485 <- {
            val pos_487 = mark
            Definition().recoverWith { case p: ParseError =>
              reset(pos_487)
              Failure(p ~ ParseFailed("expected Var 'Definition'", pos_487))
            }
          }
          catPart_486 <- {
            var parts_488 = ArrayBuffer.empty[PTree]
            var pos_489 = mark
            var res_490 = {
              val pos_491 = mark
              Definition().recoverWith { case p: ParseError =>
                reset(pos_491)
                Failure(p ~ ParseFailed("expected Var 'Definition'", pos_491))
              }
            }
            res_490.recover { _ => reset(pos_489) }
            while (res_490.isSuccess) {
              parts_488 += res_490.get
              pos_489 = mark
              res_490 = {
                val pos_492 = mark
                Definition().recoverWith { case p: ParseError =>
                  reset(pos_492)
                  Failure(p ~ ParseFailed("expected Var 'Definition'", pos_492))
                }
              }
              res_490.recover { _ => reset(pos_489) }
            }
            Try(PBranch("catPart_486", parts_488.toSeq))
          }
        } yield PBranch("catPart_480", Seq(catPart_485, catPart_486))
        res_484.recoverWith { case p: ParseError =>
          reset(pos0_483)
          Failure(p)
        }
      }
      catPart_481 <- {
        val pos_493 = mark
        EndOfFile().recoverWith { case p: ParseError =>
          reset(pos_493)
          Failure(p ~ ParseFailed("expected Var 'EndOfFile'", pos_493))
        }
      }
    } yield PBranch("Grammar", Seq(catPart_479, catPart_480, catPart_481))
    res_478.recoverWith { case p: ParseError =>
      reset(pos0_477)
      Failure(p)
    }
  }
}
