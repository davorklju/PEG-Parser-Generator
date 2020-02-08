package PEG.PEGParser

import PEG.ast.{Optional, PEGAst, Plus, Star}
import PEG.lexparse._

import scala.util.{Failure, Try}


class BasePEGParser(lexer: Lexer) extends Parser(lexer) {

  type P[a] =() => Try[a]

  def many[a](p: P[a]): Try[Vector[a]] = {
    var buf = Vector.empty[a]

    var pos0 = mark
    var x0 = p()
    x0.recover{ _ => reset(pos0) }

    while(x0.isSuccess){
      buf = buf :+ x0.get
      pos0 = mark
      x0 = p()
      x0.recover{ _ => reset(pos0) }
    }

    Try(buf)
  }

  def Grammer(): Try[Map[String,PEGAst]] = {
    val pos0 = mark
    val x0 = for{
      _ <- Spaces()
      ps <- many{ () => Definition() }
      _ <- EndOfFile()
    } yield ps.toMap
    x0.recoverWith{ case p: ParseError =>
      reset(pos0)
      Failure(p)
    }
  }

  def Definition(): Try[(String,PEGAst)] = {
    val pos0 = mark
    val x0 = for{
      PEG.ast.Var(id) <- Ident()
      _ <- LEFTARROW()
      e <- Expression()
    } yield (id,e)
    x0.recoverWith{ case p: ParseError =>
      reset(pos0)
      Failure(p)
    }
  }


  def Expression(): Try[PEGAst] = {
    def slashSeq(): Try[PEGAst] = {
      val pos0 = mark
      val x0 = SLASH()
      if(x0.isSuccess) Sequence()
      else {
        reset(pos0)
        Try( throw ParseFailed("",pos0) )
      }
    }
    val pos0 = mark
    val x0 = for{
      s0 <- Sequence()
      ss <- many{ () => slashSeq() }
    } yield s0 +: ss

    x0.map{
      case IndexedSeq(x) => x
      case xs => PEG.ast.Alt(xs)
    }
    .recoverWith{ case p: ParseError =>
      reset(pos0)
      Failure(p)
    }
  }

  def Sequence(): Try[PEGAst] = {
    var buf = Vector.empty[PEGAst]

    var pos0 = mark
    var x0 = Prefix()
    x0.recover{_ => reset(pos0)}

    while(x0.isSuccess){
      buf = buf :+ x0.get
      pos0 = mark
      x0 = Prefix()
      x0.recover{_ => reset(pos0)}
    }

    if(buf.isEmpty)
      Spaces().flatMap{ _ => Try( PEG.ast.Empty ) }
    else if(buf.tail.isEmpty) Try( buf(0))
    else Try( PEG.ast.Cat(buf) )
  }

  def Prefix(): Try[PEGAst] = {
    val pos0 = mark
    AND().flatMap{ _ => Suffix().map{x => PEG.ast.PosLook(x)} }
      .recoverWith{ case p1: ParseError =>
        reset(pos0)
        NOT().flatMap{ _ => Suffix().map{x => PEG.ast.NegLook(x)} }
          .recoverWith{ case p2: ParseError =>
            reset(pos0)
            Suffix().recoverWith{ case p3: ParseError =>
              Failure(p1~p2~p3)
            }
          }
      }
  }

  def Suffix(): Try[PEGAst] = {
    val pos0 = mark
    Primary().flatMap{ y =>
      val pos1 = mark
      QUESTION().map{ _ => Optional(y)}
        .recoverWith{ case _: ParseError =>
          reset(pos1)
          STAR().map{ _ => Star(y)}
            .recoverWith{ case _: ParseError =>
              reset(pos1)
              PLUS().map{ _ => Plus(y)}
                .recoverWith{ case _: ParseError =>
                  reset(pos1)
                  Try(y)
                }
            }
        }
    }.recoverWith{ case p: ParseError =>
      reset(pos0)
      Failure( p )
    }
  }

  def Primary(): Try[PEGAst] = {
    val pos0 = mark
    Literal().recoverWith{ case p1: ParseError =>
      reset(pos0)
      Class().recoverWith{ case p2: ParseError =>
        reset(pos0)
        DOT().map{_ => PEG.ast.Any}
          .recoverWith{ case p3: ParseError =>
            reset(pos0)
            val x1 = for{
              _ <- OPEN()
              e <- Expression()
              _ <- CLOSE()
            } yield e
            x1.recoverWith{ case p4: ParseError =>
              reset(pos0)
              val x2 = Ident().flatMap{ y =>
                val pos1 = mark
                val x3 = LEFTARROW()
                reset(pos1)
                if(x3.isSuccess) Try(throw ParseFailed("",pos1))
                else Try(y)
              }
              x2.recoverWith{ case p5:ParseError =>
                Try( throw p1 ~ p2 ~ p3 ~ p4 ~ p5)
              }
            }
          }
      }
    }
  }

  def Ident(): Try[PEGAst] = {
    val pos0 = mark
    val x0 = for{
      c0 <- IdentStart()
      cs <- many{ () => IdentPart() }
      _ <- Spaces()
    } yield PEG.ast.Var((c0 +: cs).mkString(""))
    x0.recoverWith{ case _: ParseError =>
      reset(pos0)
      Try( throw ParseFailed("Expected Ident",pos0) )
    }
  }

  def IdentStart(): Try[Char] = {
    val pos0 = mark
    val x: Try[Char] = any
    val p = x.map{ c => c == '_' || 'a'.to('z').contains(c) || 'A'.to('Z').contains(c) }
    if(p.isSuccess && p.get) x
    else {
      reset(pos0)
      Try( throw ParseFailed("Expected IdentStart",mark) )
    }
  }

  def IdentPart(): Try[Char] = {
    val pos0 = mark
    val x0 = IdentStart()
    x0.recoverWith{ case p1: ParseError =>
      reset(pos0)
      expect('0',"123456789".toList:_*).recoverWith{ case p2: ParseError =>
          Try( throw p1 ~ p2 )
      }
    }
  }


  def manyCharBetween[b,a](left: Char, right: P[b],char: P[a]): Try[Vector[a]] = {
    def charParts(): Try[a] = {
      val pos0 = mark
      val x0 = right()
      reset(pos0)
      if(x0.isSuccess){ Try( throw ParseFailed("",pos0)) }
      else {
        val x1 = char()
        x1.recoverWith{ case p: ParseError =>
          reset(pos0)
          Try(throw p)
        }
      }
    }

    for{
      _ <- expect(left)
      v <- many{ () => charParts()}
      _ <- right()
    } yield v
  }

  def Literal(): Try[PEGAst] = {
    def qqq(c: Char) =
      manyCharBetween(c, () => expect(c), () => Char())

    List(qqq('\''), qqq('"'), qqq('`'))
      .find(_.isSuccess)
      .map { tr =>
        for {
          x <- tr
          _ <- Spaces()
        } yield PEG.ast.Lit(x)
      }
      .getOrElse(Try(throw ParseFailed("Failed to parse Lit", mark)))
  }

  def Class(): Try[PEGAst] =
    manyCharBetween('[', () => expect(']'), () => Range())
      .map { l: Vector[List[Char]] =>
        val q: Set[Char] = l.map{ _.toSet[Char] }.fold(Set.empty)(_ union _)
        PEG.ast.Class(q)
      }
      .flatMap{ x =>
        Spaces().map{_ => x}
      }

  def Range(): Try[List[Char]] = {
    val pos0 = mark
    val x0 = for {
      c1 <- Char()
      _ <- expect('-')
      c2 <- Char()
    } yield c1.to(c2).toList
    if (x0.isSuccess) x0
    else {
      reset(pos0)
      Char().map { x => List(x) }
    }
  }

  def Char(): Try[Char] = {
    val pos0 = mark
    val x0 = for {
      _ <- expect('\\')
      c <- expect('n', 'r', 't', '\'', '"', '[', ']', '\\')
    } yield c match {
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case '\'' => '\''
      case '\\' => '\\'
      case _ => c
    }
    x0.recoverWith{ case p1: ParseError =>
      reset(pos0)
      val x1 = expect('\\')
      if (x1.isSuccess) {
        reset(pos0)
        Try(throw p1 ~ ParseFailed("Unexpected \\", pos0))
      } else {
        reset(pos0)
        any.recoverWith{ case p2: ParseError =>
          Try(throw p1 ~ p2)
        }
      }
    }
  }

  def LEFTARROW(): Try[Unit] = {
    val pos0 = mark
    val x0 = for {
      _ <- expect('<')
      _ <- expect('-')
      _ <- Spaces()
    } yield ()
    x0.recoverWith{ case p: ParseError =>
      reset(pos0)
      Try(throw p ~ ParseFailed("Expeced LEFTARROW", pos0))
    }
  }

  def singeCharThenWhitespace(c: Char): Try[Unit] = {
    val pos0 = mark
    val x0 = for {
      _ <- expect(c)
      _ <- Spaces()
    } yield ()
    x0.recoverWith{
      case p: ParseError =>
        reset(pos0)
        Try( throw p ~ ParseFailed(s"Expected $c",pos0))
    }
  }

  def DOT(): Try[Unit] = singeCharThenWhitespace('.')

  def CLOSE(): Try[Unit] = singeCharThenWhitespace(')')

  def OPEN(): Try[Unit] = singeCharThenWhitespace('(')

  def PLUS(): Try[Unit] = singeCharThenWhitespace('+')

  def STAR(): Try[Unit] = singeCharThenWhitespace('*')

  def QUESTION(): Try[Unit] = singeCharThenWhitespace('?')

  def NOT(): Try[Unit] = singeCharThenWhitespace('!')

  def AND(): Try[Unit] = singeCharThenWhitespace('&')

  def SLASH(): Try[Unit] = singeCharThenWhitespace('/')

  def Spaces(): Try[Unit] = {
    def spaceOrComment: Try[Unit] = {
      val pos0 = mark
      val x0 = Space()
      x0.recoverWith{ case p1: ParseError =>
        reset(pos0)
        val x1 = Comment()
        x1.recoverWith{ case p2: ParseError =>
          reset(pos0)
          Try(throw p1 ~ p2 ~ ParseFailed("Expected Space or Comment", pos0))
        }
      }
    }

    var pos0 = mark
    var x0 = spaceOrComment
    x0.recover{ case _ => reset(pos0)}
    while (x0.isSuccess) {
      pos0 = mark
      x0 = spaceOrComment
      x0.recover{ case _ => reset(pos0)}
    }
    Try(())
  }

  def Comment(): Try[Unit] = {
    val pos0 = mark
    val x1 = for {
      _ <- expect('#').map { _ => () }
      _ <- {
        var x2 = EndOfLine()
        while (x2.isFailure) {
          val pos1 = mark
          val x3 = any.map { _ => () }
          x3.recover{case _ =>
            reset(pos1)
            x2 = x3
          }
          x3.foreach{ _ =>
            val pos2 = mark
            val x4 = EndOfLine()
            x2 = x4.map{ x => reset(pos2); x}
          }
        }
        Try(())
      }
      _ <- EndOfLine()
    } yield ()

    x1.recoverWith{ case p: ParseError =>
      reset(pos0)
      Try(throw p ~ ParseFailed("Expected comment", pos0))
    }
  }

  def Space(): Try[Unit] = {
    val pos0 = mark
    val x1 = expect(' ', '\t').map { _ => () }
    x1.recoverWith{ case _ =>
      reset(pos0)
      EndOfLine()
    }
  }

  def EndOfLine(): Try[Unit] = {
    val pos0 = mark
    val x0 = for {
      _ <- expect('\r')
      _ <- expect('\n')
    } yield ()
    x0.recoverWith{ case p1: ParseError =>
      reset(pos0)
      val x1 = expect('\r', '\n').map { _ => () }
      x1.recoverWith{ case p2: ParseError =>
        reset(pos0)
        Try { throw p1 ~ p2 ~ ParseFailed("expected", pos0) }
      }
    }
  }

  def EndOfFile(): Try[Unit] = {
    val pos0 = mark
    val x0 = lookAhead
    reset(pos0)
    if (x0.isSuccess) Try(throw ParseFailed("Expected EOF", pos0))
    else Try(())
  }

}
