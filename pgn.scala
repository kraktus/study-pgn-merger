//> using scala "3.3.1"
//> using dep "org.typelevel::toolkit:latest.release"
//> using repository "https://raw.githubusercontent.com/lichess-org/lila-maven/master"
//> using dep "org.lichess::scalachess:15.6.7"
//> using dep "dev.optics::monocle-core:3.2.0"
//> using dep "dev.optics::monocle-macro:3.2.0"

import cats.syntax.all.*
import cats.effect.*
import monocle.syntax.all.*

import chess.format.pgn.*
import chess.{Tree, HasId, Mergeable}

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

def parse(s: String): Tree[PgnNodeData] =
  val parsed = Parser.full(PgnStr(game1)).toOption.get
  parsed.tree.get

object Hello extends IOApp.Simple:
  def run =
    val g1   = parse(game1)
    val g2   = parse(game2)
    // count total nodes in the game
    IO.println(g1.foldLeft(0)((b, _) => b + 1))
    // print merged tree
    IO.println(Tree.merge(g1.child, g2.child).get.toPgn)