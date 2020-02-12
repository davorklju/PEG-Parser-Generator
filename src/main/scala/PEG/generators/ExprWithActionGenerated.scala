package PEG.generators

import PEG.lexparse.{Lexer, Parser}
import PEG.data.implicits._
import PEG.data._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}


class ExprWithActionGenerated(lexer: Lexer) extends Parser(lexer) {


  def Stmt(): Try[Int] = {
    val pos_1 = mark
    val res_2 = for {
      _ <- WS()
      e <- Expr()
      _ <- EOF()
    } yield (e)
    res_2.recoverWith { case p: ParseError[Char] =>
      reset(pos_1)
      Failure(p)
    }
  }


  def Expr(): Try[Int] = {
    val pos_3 = mark
    val res_4 = {
      val pos_6 = mark
      val res_7 = for {
        a <- Fact()
        _ <- PLUS()
        b <- Expr()
      } yield (a + b)
      res_7.recoverWith { case p: ParseError[Char] =>
        reset(pos_6)
        Failure(p)
      }
    }
    res_4.recoverWith { case err_5: ParseError[Char] =>
      reset(pos_3)
      val res_8 = {
        val pos_10 = mark
        val res_11 = for {
          a <- Fact()
          _ <- MINUS()
          b <- Expr()
        } yield (a - b)
        res_11.recoverWith { case p: ParseError[Char] =>
          reset(pos_10)
          Failure(p)
        }
      }
      res_8.recoverWith { case err_9: ParseError[Char] =>
        reset(pos_3)
        Fact().recoverWith { case err_12: ParseError[Char] =>
          reset(pos_3)
          Failure(err_5 ~ err_9 ~ err_12 ~ ParseFailed("", pos_3))
        }
      }
    }
  }


  val cache_14 = mutable.HashMap.empty[Int, (Try[Int], Int)]

  def Fact(): Try[Int] = {
    def parser_13(): Try[Int] = {
      val pos_18 = mark
      val res_19 = {
        val pos_21 = mark
        val res_22 = for {
          a <- Lit()
          _ <- TIMES()
          b <- Fact()
        } yield (a * b)
        res_22.recoverWith { case p: ParseError[Char] =>
          reset(pos_21)
          Failure(p)
        }
      }
      res_19.recoverWith { case err_20: ParseError[Char] =>
        reset(pos_18)
        Lit().recoverWith { case err_23: ParseError[Char] =>
          reset(pos_18)
          Failure(err_20 ~ err_23 ~ ParseFailed("", pos_18))
        }
      }
    }

    if (!cache_14.contains(mark)) {
      val init_16 = mark
      cache_14(init_16) = parser_13() -> mark
      reset(init_16)
    }
    val (res_15, pos_17) = cache_14(mark)
    reset(pos_17)
    res_15
  }


  val cache_25 = mutable.HashMap.empty[Int, (Try[Int], Int)]

  def Lit(): Try[Int] = {
    def parser_24(): Try[Int] = {
      val pos_29 = mark
      val res_30 = {
        val pos_32 = mark
        val res_33 = for {
          _ <- OPEN()
          e <- Expr()
          _ <- CLOSE()
        } yield (e)
        res_33.recoverWith { case p: ParseError[Char] =>
          reset(pos_32)
          Failure(p)
        }
      }
      res_30.recoverWith { case err_31: ParseError[Char] =>
        reset(pos_29)
        val res_34 = {
          val pos_36 = mark
          val res_37 = for {
            _ <- MINUS()
            e <- Lit()
          } yield (-e)
          res_37.recoverWith { case p: ParseError[Char] =>
            reset(pos_36)
            Failure(p)
          }
        }
        res_34.recoverWith { case err_35: ParseError[Char] =>
          reset(pos_29)
          Int().recoverWith { case err_38: ParseError[Char] =>
            reset(pos_29)
            Failure(err_31 ~ err_35 ~ err_38 ~ ParseFailed("", pos_29))
          }
        }
      }
    }

    if (!cache_25.contains(mark)) {
      val init_27 = mark
      cache_25(init_27) = parser_24() -> mark
      reset(init_27)
    }
    val (res_26, pos_28) = cache_25(mark)
    reset(pos_28)
    res_26
  }


  def Int(): Try[Int] = {
    val pos_39 = mark
    val res_40 = for {
      PBranch(_, xs) <- {
        val pos0_41 = mark
        val res_42 = for {
          catPart_43 <- expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3').map { char_45 => PLeaf(char_45.toString) }
          catPart_44 <- {
            def catPart_44_sub_49 = {
              val pos_50 = mark
              expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
                .map { char_51 => PLeaf(char_51.toString) }
                .recoverWith { case p: ParseError[Char] =>
                  reset(pos_50)
                  Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_50))
                }
            }

            var buf_46 = ArrayBuffer.empty[PTree]
            var pos_47 = mark
            var res_48 = catPart_44_sub_49
            res_48.recover { _ => reset(pos_47) }
            while (res_48.isSuccess) {
              buf_46 += res_48.get
              pos_47 = mark
              res_48 = catPart_44_sub_49
              res_48.recover { _ => reset(pos_47) }
            }
            Try(PBranch("catPart_44", buf_46.toSeq))
          }
        } yield PBranch("PBranch(_,xs)", Seq(catPart_43, catPart_44))
        res_42.recoverWith { case p: ParseError[Char] =>
          reset(pos0_41)
          Failure(p)
        }
      }
      _ <- WS()
    } yield (xs.map(PEGGenerator.flattenNoWS).mkString("").toInt)
    res_40.recoverWith { case p: ParseError[Char] =>
      reset(pos_39)
      Failure(p)
    }
  }


  def MINUS(): Try[PTree] = {
    val pos0_52 = mark
    val res_53 = for {
      catPart_54 <- expect('-').map { char_56 => PLeaf(char_56.toString) }
      catPart_55 <- WS()
    } yield PBranch("MINUS", Seq(catPart_54, catPart_55))
    res_53.recoverWith { case p: ParseError[Char] =>
      reset(pos0_52)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_57 = mark
    val res_58 = for {
      catPart_59 <- expect('+').map { char_61 => PLeaf(char_61.toString) }
      catPart_60 <- WS()
    } yield PBranch("PLUS", Seq(catPart_59, catPart_60))
    res_58.recoverWith { case p: ParseError[Char] =>
      reset(pos0_57)
      Failure(p)
    }
  }


  def TIMES(): Try[PTree] = {
    val pos0_62 = mark
    val res_63 = for {
      catPart_64 <- expect('*').map { char_66 => PLeaf(char_66.toString) }
      catPart_65 <- WS()
    } yield PBranch("TIMES", Seq(catPart_64, catPart_65))
    res_63.recoverWith { case p: ParseError[Char] =>
      reset(pos0_62)
      Failure(p)
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_67 = mark
    val res_68 = for {
      catPart_69 <- expect('(').map { char_71 => PLeaf(char_71.toString) }
      catPart_70 <- WS()
    } yield PBranch("OPEN", Seq(catPart_69, catPart_70))
    res_68.recoverWith { case p: ParseError[Char] =>
      reset(pos0_67)
      Failure(p)
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_72 = mark
    val res_73 = for {
      catPart_74 <- expect(')').map { char_76 => PLeaf(char_76.toString) }
      catPart_75 <- WS()
    } yield PBranch("CLOSE", Seq(catPart_74, catPart_75))
    res_73.recoverWith { case p: ParseError[Char] =>
      reset(pos0_72)
      Failure(p)
    }
  }


  def WS(): Try[PTree] = {
    def WS_sub_80 = {
      val pos_81 = mark
      expect(' ', '\t', '\n', '\r')
        .map { char_82 => PLeaf(char_82.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_81)
          Failure(p ~ ParseFailed("Expected one of ' ','\t','\n','\r'", pos_81))
        }
    }

    var buf_77 = ArrayBuffer.empty[PTree]
    var pos_78 = mark
    var res_79 = WS_sub_80
    res_79.recover { _ => reset(pos_78) }
    while (res_79.isSuccess) {
      buf_77 += res_79.get
      pos_78 = mark
      res_79 = WS_sub_80
      res_79.recover { _ => reset(pos_78) }
    }
    Try(PBranch("WS", buf_77.toSeq))
  }


  def EOF(): Try[PTree] = {
    val pos_83 = mark
    val res_84 = {
      val pos_85 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_85)
          Failure(p ~ ParseFailed("Expected any char", pos_85))
        }
    }
    reset(pos_83)
    if (res_84.isSuccess) Failure(ParseFailed("Neglook failed", pos_83))
    else {
      Try(PEmpty)
    }
  }


  def QQQ(): Try[List[Int]] = {
    val pos_86 = mark
    val res_87 = for {
      _ <- WS()
      x <- Start()
      _ <- EOF()
    } yield (x)
    res_87.recoverWith { case p: ParseError[Char] =>
      reset(pos_86)
      Failure(p)
    }
  }


  def Start(): Try[List[Int]] = {
    val pos_88 = mark
    val res_89 = for {
      x <- Int()
      y <- Follow()
    } yield (x :: y)
    res_89.recoverWith { case p: ParseError[Char] =>
      reset(pos_88)
      Failure(p)
    }
  }


  def Follow(): Try[List[Int]] = {
    val pos_90 = mark
    val res_91 = {
      val pos_93 = mark
      val res_94 = for {
        _ <- PLUS()
        x <- Start()
      } yield (x)
      res_94.recoverWith { case p: ParseError[Char] =>
        reset(pos_93)
        Failure(p)
      }
    }
    res_91.recoverWith { case err_92: ParseError[Char] =>
      reset(pos_90)
      val res_95 = {
        Try {
          Nil
        }
      }
      res_95.recoverWith { case err_96: ParseError[Char] =>
        reset(pos_90)
        Failure(err_92 ~ err_96 ~ ParseFailed("", pos_90))
      }
    }
  }


  def IE(): Try[Expr] = {
    {
      val pos_97 = mark
      Int().recoverWith { case p: ParseError[Char] =>
        reset(pos_97)
        Failure(p ~ ParseFailed("expected Var 'Int'", pos_97))
      }
      }
      .map { case (x) =>
        I(x)
      }
  }


  def LE(): Try[Expr] = {
    val pos_98 = mark
    val res_99 = {
      val pos_101 = mark
      val res_102 = for {
        _ <- MINUS()
        x <- EE()
      } yield (Neg(x))
      res_102.recoverWith { case p: ParseError[Char] =>
        reset(pos_101)
        Failure(p)
      }
    }
    res_99.recoverWith { case err_100: ParseError[Char] =>
      reset(pos_98)
      val res_103 = {
        val pos_105 = mark
        val res_106 = for {
          _ <- OPEN()
          x <- EE()
          _ <- CLOSE()
        } yield (x)
        res_106.recoverWith { case p: ParseError[Char] =>
          reset(pos_105)
          Failure(p)
        }
      }
      res_103.recoverWith { case err_104: ParseError[Char] =>
        reset(pos_98)
        IE().recoverWith { case err_107: ParseError[Char] =>
          reset(pos_98)
          Failure(err_100 ~ err_104 ~ err_107 ~ ParseFailed("", pos_98))
        }
      }
    }
  }


  def FE(): Try[Expr] = {
    val pos_108 = mark
    val res_109 = {
      val pos_111 = mark
      val res_112 = for {
        a <- LE()
        _ <- TIMES()
        b <- FE()
      } yield (Mul(a, b))
      res_112.recoverWith { case p: ParseError[Char] =>
        reset(pos_111)
        Failure(p)
      }
    }
    res_109.recoverWith { case err_110: ParseError[Char] =>
      reset(pos_108)
      LE().recoverWith { case err_113: ParseError[Char] =>
        reset(pos_108)
        Failure(err_110 ~ err_113 ~ ParseFailed("", pos_108))
      }
    }
  }


  def EE(): Try[Expr] = {
    val pos_114 = mark
    val res_115 = {
      val pos_117 = mark
      val res_118 = for {
        a <- FE()
        _ <- PLUS()
        b <- EE()
      } yield (Add(a, b))
      res_118.recoverWith { case p: ParseError[Char] =>
        reset(pos_117)
        Failure(p)
      }
    }
    res_115.recoverWith { case err_116: ParseError[Char] =>
      reset(pos_114)
      FE().recoverWith { case err_119: ParseError[Char] =>
        reset(pos_114)
        Failure(err_116 ~ err_119 ~ ParseFailed("", pos_114))
      }
    }
  }
}
