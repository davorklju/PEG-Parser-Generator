package PEG.PEGParser

import PEG.data.{PBranch, PEmpty, PLeaf, PTree}
import PEG.lexparse.{Lexer , Parser}
import PEG.data._
import PEG.data.implicits._

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

class ExprGenerated(lexer: Lexer) extends Parser(lexer) {

  def Stmt(): Try[PTree] = {
    val pos0_70 = mark
    val res_71 = for {
      catPart_72 <- WS()
      catPart_73 <- Expr()
      catPart_74 <- EOF()
    } yield PBranch("Stmt", Seq(catPart_72, catPart_73, catPart_74))
    res_71.recoverWith { case p: ParseError[Char] =>
      reset(pos0_70)
      Failure(p)
    }
  }

  def Expr(): Try[PTree] = {
    val pos_8 = mark
    val res_9 = {
      val pos0_11 = mark
      val res_12 = for {
        catPart_13 <- Fact()
        catPart_14 <- PLUS()
        catPart_15 <- Expr()
      } yield PBranch("Expr", Seq(catPart_13, catPart_14, catPart_15))
      res_12.recoverWith { case p: ParseError[Char] =>
        reset(pos0_11)
        Failure(p)
      }
    }
    res_9.recoverWith { case err_10: ParseError[Char] =>
      reset(pos_8)
      Fact().recoverWith { case err_16: ParseError[Char] =>
        reset(pos_8)
        Failure(err_10 ~ err_16 ~ ParseFailed("", pos_8))
      }
    }
  }

  def Fact(): Try[PTree] = {
    val pos_27 = mark
    val res_28 = {
      val pos0_30 = mark
      val res_31 = for {
        catPart_32 <- Term()
        catPart_33 <- PROD()
        catPart_34 <- Fact()
      } yield PBranch("Fact", Seq(catPart_32, catPart_33, catPart_34))
      res_31.recoverWith { case p: ParseError[Char] =>
        reset(pos0_30)
        Failure(p)
      }
    }
    res_28.recoverWith { case err_29: ParseError[Char] =>
      reset(pos_27)
      Term().recoverWith { case err_35: ParseError[Char] =>
        reset(pos_27)
        Failure(err_29 ~ err_35 ~ ParseFailed("", pos_27))
      }
    }
  }

  def Term(): Try[PTree] = {
    val pos0_1 = mark
    val res_2 = for {
      catPart_3 <- {
        val pos_5 = mark
        expect('-').map { char_6 => PLeaf(char_6.toString) }
          .recoverWith { case err_7: ParseError[Char] =>
            reset(pos_5)
            Try(PEmpty)
          }
      }
      catPart_4 <- Lit()
    } yield PBranch("Term", Seq(catPart_3, catPart_4))
    res_2.recoverWith { case p: ParseError[Char] =>
      reset(pos0_1)
      Failure(p)
    }
  }

  def Lit(): Try[PTree] = {
    val pos_46 = mark
    val res_47 = for {
        catPart_51 <- OPEN()
        catPart_52 <- Expr()
        catPart_53 <- CLOSE()
      } yield PBranch("Lit", Seq(catPart_51, catPart_52, catPart_53))
    res_47.recoverWith { case err_48: ParseError[Char] =>
      reset(pos_46)
      Int().recoverWith { case err_54: ParseError[Char] =>
        reset(pos_46)
        Failure(err_48 ~ err_54 ~ ParseFailed("", pos_46))
      }
    }
  }

  def Int(): Try[PTree] = {
    val pos0_55 = mark
    val res_56 = for {
      catPart_57 <- {
        val pos0_59 = mark
        val res_60 = for {
          catPart_61 <- expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3').map { char_63 => PLeaf(char_63.toString) }
          catPart_62 <- {
            def subMatch_67 = {
              val pos_68 = mark
              expect('6', '9', '2', '8', '4', '0', '5', '1', '7', '3')
                .map { char_69 => PLeaf(char_69.toString) }
                .recoverWith { case p: ParseError[Char] =>
                  reset(pos_68)
                  Failure(p ~ ParseFailed("Expected one of '6','9','2','8','4','0','5','1','7','3'", pos_68))
                }
            }

            var buf_64 = ArrayBuffer.empty[PTree]
            var pos_65 = mark
            var res_66 = subMatch_67
            res_66.recover { _ => reset(pos_65) }
            while (res_66.isSuccess) {
              buf_64 += res_66.get
              pos_65 = mark
              res_66 = subMatch_67
              res_66.recover { _ => reset(pos_65) }
            }
            Try(PBranch("catPart_62", buf_64.toSeq))
          }
        } yield PBranch("catPart_57", Seq(catPart_61, catPart_62))
        res_60.recoverWith { case p: ParseError[Char] =>
          reset(pos0_59)
          Failure(p)
        }
      }
      catPart_58 <- WS()
    } yield PBranch("Int", Seq(catPart_57, catPart_58))
    res_56.recoverWith { case p: ParseError[Char] =>
      reset(pos0_55)
      Failure(p)
    }
  }


  def PLUS(): Try[PTree] = {
    val pos0_17 = mark
    val res_18 = for {
      catPart_19 <- expect('+').map { char_21 => PLeaf(char_21.toString) }
      catPart_20 <- WS()
    } yield PBranch("PLUS", Seq(catPart_19, catPart_20))
    res_18.recoverWith { case p: ParseError[Char] =>
      reset(pos0_17)
      Failure(p)
    }
  }

  def PROD(): Try[PTree] = {
    val pos0_41 = mark
    val res_42 = for {
      catPart_43 <- expect('*').map { char_45 => PLeaf(char_45.toString) }
      catPart_44 <- WS()
    } yield PBranch("PROD", Seq(catPart_43, catPart_44))
    res_42.recoverWith { case p: ParseError[Char] =>
      reset(pos0_41)
      Failure(p)
    }
  }

  def OPEN(): Try[PTree] = {
    val pos0_36 = mark
    val res_37 = for {
      catPart_38 <- expect('(').map { char_40 => PLeaf(char_40.toString) }
      catPart_39 <- WS()
    } yield PBranch("OPEN", Seq(catPart_38, catPart_39))
    res_37.recoverWith { case p: ParseError[Char] =>
      reset(pos0_36)
      Failure(p)
    }
  }

  def CLOSE(): Try[PTree] = {
    val pos0_22 = mark
    val res_23 = for {
      catPart_24 <- expect(')').map { char_26 => PLeaf(char_26.toString) }
      catPart_25 <- WS()
    } yield PBranch("CLOSE", Seq(catPart_24, catPart_25))
    res_23.recoverWith { case p: ParseError[Char] =>
      reset(pos0_22)
      Failure(p)
    }
  }

  def WS(): Try[PTree] = {
    def subMatch_81 = {
      val pos_82 = mark
      expect(' ', '\t', '\n', '\r')
        .map { char_83 => PLeaf(char_83.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_82)
          Failure(p ~ ParseFailed("Expected one of ' ','\t','\n','\r'", pos_82))
        }
    }

    var buf_78 = ArrayBuffer.empty[PTree]
    var pos_79 = mark
    var res_80 = subMatch_81
    res_80.recover { _ => reset(pos_79) }
    while (res_80.isSuccess) {
      buf_78 += res_80.get
      pos_79 = mark
      res_80 = subMatch_81
      res_80.recover { _ => reset(pos_79) }
    }
    Try(PBranch("WS", buf_78.toSeq))
  }

  def EOF(): Try[PTree] = {
    val pos_75 = mark
    val res_76 = {
      val pos_77 = mark
      any.map { x => PLeaf(x.toString) }
        .recoverWith { case p: ParseError[Char] =>
          reset(pos_77)
          Failure(p ~ ParseFailed("Expected any char", pos_77))
        }
    }
    reset(pos_75)
    if (res_76.isSuccess) Failure(ParseFailed("Neglook failed", pos_75))
    else {
      Try(PEmpty)
    }
  }
}
