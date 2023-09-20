//> using scala "3.3.1"
//> using dep "org.typelevel::toolkit:latest.release"
//> using repository "https://raw.githubusercontent.com/lichess-org/lila-maven/master"
//> using dep "org.lichess::scalachess:15.6.7"

import cats.syntax.all.*

import cats.effect.*
import monocle.syntax.all.*

import chess.format.pgn.*
import chess.{Node, Tree, HasId, Mergeable}

val game1 = """
  1. e4 e5
  """

val game2 = """
  1. e4 c5
  """

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

object Hello extends IOApp.Simple:
  import PgnHelper.*
  def run =
    val g1   = parse(game1)
    val g2   = parse(game2)
    // count total nodes in the game
    // IO.println(g1.foldLeft(0)((b, _) => b + 1))
    // print merged tree
    IO.println(g1.toPgn.render) >>
    IO.println(g2.toPgn.render) >>
    IO.println(merge(g1, g2).toPgn.render)


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


  private def makeGame(tags: Tags) =
    val g = Game(
      variantOption = tags(_.Variant) flatMap chess.variant.Variant.byName,
      fen = tags.fen
    )
    g.copy(
      startedAtPly = g.ply,
      clock = tags.clockConfig map Clock.apply
    )