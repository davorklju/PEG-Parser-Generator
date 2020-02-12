package PEG.generators

import PEG.ast.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer, ParseError, ParseFailed, Parser}
import PEG.lexparse.ParseError.implicits._
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


  def Fact(): Try[Int] = {
    val pos_13 = mark
    val res_14 = {
      val pos_16 = mark
      val res_17 = for {
        a <- Lit()
        _ <- TIMES()
        b <- Fact()
      } yield (a * b)
      res_17.recoverWith { case p: ParseError[Char] =>
        reset(pos_16)
        Failure(p)
      }
    }
    res_14.recoverWith { case err_15: ParseError[Char] =>
      reset(pos_13)
      Lit().recoverWith { case err_18: ParseError[Char] =>
        reset(pos_13)
        Failure(err_15 ~ err_18 ~ ParseFailed("", pos_13))
      }
    }
  }


  def Lit(): Try[Int] = {
    val pos_19 = mark
    val res_20 = {
      val pos_22 = mark
      val res_23 = for {
        _ <- OPEN()
        e <- Expr()
        _ <- CLOSE()
      } yield (e)
      res_23.recoverWith { case p: ParseError[Char] =>
        reset(pos_22)
        Failure(p)
      }
    }
    res_20.recoverWith { case err_21: ParseError[Char] =>
      reset(pos_19)
      val res_24 = {
        val pos_26 = mark
        val res_27 = for {
          _ <- MINUS()
          e <- Lit()
        } yield (-e)
        res_27.recoverWith { case p: ParseError[Char] =>
          reset(pos_26)
          Failure(p)
        }
      }
      res_24.recoverWith { case err_25: ParseError[Char] =>
        reset(pos_19)
        Int().recoverWith { case err_28: ParseError[Char] =>
          reset(pos_19)
          Failure(err_21 ~ err_25 ~ err_28 ~ ParseFailed("", pos_19))
        }
      }
    }
  }


  val cache_30 = mutable.HashMap.empty[Int, (Try[Int], Int)]

  def Int(): Try[Int] = {
    def parser_29(): Try[Int] = {
      val pos_34 = mark
      val res_35 = for {
        xs <- {
          val pos0_36 = mark
          val res_37 = for {
            catPart_38 <- expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3').map { char_40 => PLeaf(char_40.toString) }
            catPart_39 <- {
              def subMatch_44 = {
                val pos_45 = mark
                expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
                  .map { char_46 => PLeaf(char_46.toString) }
                  .recoverWith { case p: ParseError[Char] =>
                    reset(pos_45)
                    Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_45))
                  }
              }

              var buf_41 = ArrayBuffer.empty[PTree]
              var pos_42 = mark
              var res_43 = subMatch_44
              res_43.recover { _ => reset(pos_42) }
              while (res_43.isSuccess) {
                buf_41 += res_43.get
                pos_42 = mark
                res_43 = subMatch_44
                res_43.recover { _ => reset(pos_42) }
              }
              Try(PBranch("catPart_39", buf_41.toSeq))
            }
          } yield PBranch("xs", Seq(catPart_38, catPart_39))
          res_37.recoverWith { case p: ParseError[Char] =>
            reset(pos0_36)
            Failure(p)
          }
        }
        _ <- WS()
      } yield (PEGGenerator.flattenNoWS(xs).toInt)
      res_35.recoverWith { case p: ParseError[Char] =>
        reset(pos_34)
        Failure(p)
      }
    }

    if (!cache_30.contains(mark)) {
      val init_32 = mark
      cache_30(init_32) = parser_29() -> mark
      reset(init_32)
    }
    val (res_31, pos_33) = cache_30(mark)
    reset(pos_33)
    res_31
  }


  def MINUS(): Try[PTree] = {
    val pos0_47 = mark
    val res_48 = for {
      catPart_49 <- expect('-').map { char_51 => PLeaf(char_51.toString) }
      catPart_50 <- WS()
    } yield PBranch("MINUS", Seq(catPart_49, catPart_50))
    res_48.recoverWith { case p: ParseError[Char] =>
      reset(pos0_47)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_52 = mark
    val res_53 = for {
      catPart_54 <- expect('+').map { char_56 => PLeaf(char_56.toString) }
      catPart_55 <- WS()
    } yield PBranch("PLUS", Seq(catPart_54, catPart_55))
    res_53.recoverWith { case p: ParseError[Char] =>
      reset(pos0_52)
      Failure(p)
    }
  }


  def TIMES(): Try[PTree] = {
    val pos0_57 = mark
    val res_58 = for {
      catPart_59 <- expect('*').map { char_61 => PLeaf(char_61.toString) }
      catPart_60 <- WS()
    } yield PBranch("TIMES", Seq(catPart_59, catPart_60))
    res_58.recoverWith { case p: ParseError[Char] =>
      reset(pos0_57)
      Failure(p)
    }
  }


  def OPEN(): Try[PTree] = {
    val pos0_62 = mark
    val res_63 = for {
      catPart_64 <- expect('(').map { char_66 => PLeaf(char_66.toString) }
      catPart_65 <- WS()
    } yield PBranch("OPEN", Seq(catPart_64, catPart_65))
    res_63.recoverWith { case p: ParseError[Char] =>
      reset(pos0_62)
      Failure(p)
    }
  }


  def CLOSE(): Try[PTree] = {
    val pos0_67 = mark
    val res_68 = for {
      catPart_69 <- expect(')').map { char_71 => PLeaf(char_71.toString) }
      catPart_70 <- WS()
    } yield PBranch("CLOSE", Seq(catPart_69, catPart_70))
    res_68.recoverWith { case p: ParseError[Char] =>
      reset(pos0_67)
      Failure(p)
    }
  }


  def WS(): Try[PTree] = {
    def subMatch_75 = {
      val pos_76 = mark
      expect(' ', '\t', '\n', '\r')
        .map { char_77 => PLeaf(char_77.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_76)
          Failure(p ~ ParseFailed("Expected one of ' ','\t','\n','\r'", pos_76))
        }
    }

    var buf_72 = ArrayBuffer.empty[PTree]
    var pos_73 = mark
    var res_74 = subMatch_75
    res_74.recover { _ => reset(pos_73) }
    while (res_74.isSuccess) {
      buf_72 += res_74.get
      pos_73 = mark
      res_74 = subMatch_75
      res_74.recover { _ => reset(pos_73) }
    }
    Try(PBranch("WS", buf_72.toSeq))
  }


  def EOF(): Try[PTree] = {
    val pos_78 = mark
    val res_79 = {
      val pos_80 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_80)
          Failure(p ~ ParseFailed("Expected any char", pos_80))
        }
    }
    reset(pos_78)
    if (res_79.isSuccess) Failure(ParseFailed("Neglook failed", pos_78))
    else {
      Try(PEmpty)
    }
  }


  def QQQ(): Try[List[Int]] = {
    val pos_81 = mark
    val res_82 = for {
      _ <- WS()
      x <- Start()
      _ <- EOF()
    } yield (x)
    res_82.recoverWith { case p: ParseError[Char] =>
      reset(pos_81)
      Failure(p)
    }
  }


  def Start(): Try[List[Int]] = {
    val pos_83 = mark
    val res_84 = for {
      x <- Int()
      y <- Follow()
    } yield (x :: y)
    res_84.recoverWith { case p: ParseError[Char] =>
      reset(pos_83)
      Failure(p)
    }
  }


  def Follow(): Try[List[Int]] = {
    val pos_85 = mark
    val res_86 = {
      val pos_88 = mark
      val res_89 = for {
        _ <- PLUS()
        x <- Start()
      } yield (x)
      res_89.recoverWith { case p: ParseError[Char] =>
        reset(pos_88)
        Failure(p)
      }
    }
    res_86.recoverWith { case err_87: ParseError[Char] =>
      reset(pos_85)
      val res_90 = { Try { Nil } }
      res_90.recoverWith { case err_91: ParseError[Char] =>
        reset(pos_85)
        Failure(err_87 ~ err_91 ~ ParseFailed("", pos_85))
      }
    }
  }
}
