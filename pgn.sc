#!/usr/bin/env -S scala-cli shebang
//> using scala "3.3.1"
//> using dep "org.typelevel::toolkit:latest.release"
//> using repository "https://raw.githubusercontent.com/lichess-org/lila-maven/master"
//> using dep "org.lichess::scalachess:15.6.7"

import cats.syntax.all.*
import scala.io.Source
import java.io.*

import chess.format.pgn.*
import chess.{Node, Tree, HasId, Mergeable}

object PgnHelper:
  import chess.MoveOrDrop.*
  import chess.{Clock, Situation, Game, Ply}
  import chess.format.pgn.Move
  case class Context(sit: Situation, ply: Ply)

  extension (d: PgnNodeData)
    def toMove(context: Context): Option[(Situation, Move)] =
      d.san(context.sit)
        .toOption
        .map(x =>
          (
            x.situationAfter,
            Move(
              ply = context.ply,
              san = x.toSanStr,
              comments = d.comments,
              glyphs = d.glyphs,
              opening = None,
              result = None,
              secondsLeft = None,
              variationComments = d.variationComments
            )
          )
        )

  extension (tree: ParsedPgnTree)
    def toPgn(game: Game): Option[PgnTree] =
      tree.mapAccumlOption_(Context(game.situation, game.ply + 1)): (ctx, d) =>
        d.toMove(ctx) match
          case Some((sit, m)) => (Context(sit, ctx.ply.next), m.some)
          case None           => (ctx, None)

  extension (pgn: ParsedPgn)
    def toPgn: Pgn =
      val game = makeGame(pgn.tags)
      Pgn(pgn.tags, pgn.initialPosition, pgn.tree.flatMap(_.toPgn(game)))

    def onlyMoves: ParsedPgn =
      ParsedPgn(
        pgn.initialPosition,
        pgn.tags,
        pgn.tree.map(
          _.map(nodeData => PgnNodeData(nodeData.san, Metas.empty, Nil))
        )
      )

  private def makeGame(tags: Tags) =
    val g = Game(
      variantOption = tags(_.Variant) flatMap chess.variant.Variant.byName,
      fen = tags.fen
    )
    g.copy(
      startedAtPly = g.ply,
      clock = tags.clockConfig map Clock.apply
    )

val game1 = """
  1. e4 e5
  """

val game2 = """
  1. e4 c5
  """

def writeFile(filename: String, s: String): Unit = {
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(s)
  bw.close()
}

given HasId[PgnNodeData, San] with
  extension (a: PgnNodeData) def id: San = a.san
given Mergeable[PgnNodeData] with
  extension (x: PgnNodeData)
    // don't care about the `Metas` for the moment
    def merge(y: PgnNodeData): Option[PgnNodeData] =
      if x.id == y.id then PgnNodeData(x.san, Metas.empty, Nil).some
      else None

def parse(s: String): ParsedPgn =
  Parser.full(PgnStr(s)).toOption.get

def merge(t1: ParsedPgn, t2: ParsedPgn): ParsedPgn =
  // ignore comments & tags for now
  ParsedPgn(InitialComments.empty, Tags.empty, Tree.merge(t1.tree, t2.tree))

def main(args: List[String]) =
  import PgnHelper.{toPgn, onlyMoves}
  val inputFile = args
    .get(0)
    .getOrElse(throw new RuntimeException("no path to PGN file found"))
  val outputFile = args.get(1).getOrElse("theOne.pgn")
  val pgns = Source.fromFile(inputFile).mkString.split("\n\n\n").map(parse).map(_.onlyMoves)
  val theOne = pgns.fold(parse(""))(merge(_, _))
  writeFile(outputFile, theOne.toPgn.render.value)

main(args.toList)
