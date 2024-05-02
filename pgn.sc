#!/usr/bin/env -S scala-cli shebang
//> using scala "3.4.1"
//> using dep "org.typelevel::cats-core:2.10.0"
//> using repository "https://raw.githubusercontent.com/lichess-org/lila-maven/master"
//> using dep "org.lichess::scalachess:16.0.6"

import cats.syntax.all.*
import scala.io.Source
import java.io.*

import chess.format.pgn.*
import chess.{ HasId, Mergeable, Node, Tree }

object PgnHelper:

  extension (pgn: ParsedPgn)
    def onlyMoves: ParsedPgn =
      ParsedPgn(
        pgn.initialPosition,
        pgn.tags,
        pgn.tree.map(
          _.map(nodeData => PgnNodeData(nodeData.san, Metas.empty, Nil))
        )
      )

val game1 = """
  1. e4 e5
  """

val game2 = """
  1. e4 c5
  """

def writeFile(filename: String, s: String): Unit = {
  val file = new File(filename)
  val bw   = new BufferedWriter(new FileWriter(file))
  bw.write(s)
  bw.close()
}

given HasId[PgnNodeData, San] with
  extension (a: PgnNodeData) def id: San = a.san
given Mergeable[PgnNodeData] with
  extension (x: PgnNodeData)
    // don't care about the `Metas` for the moment
    def merge(y: PgnNodeData): Option[PgnNodeData] =
      Option.when(x.id == y.id)(PgnNodeData(x.san, Metas.empty, Nil))

def parse(s: String): ParsedPgn =
  Parser.full(PgnStr(s)).toOption.get

def merge(t1: ParsedPgn, t2: ParsedPgn): ParsedPgn =
  // arbitrarily keep t1 tags and initial comments
  ParsedPgn(t1.initialPosition, t1.tags, Tree.merge(t1.tree, t2.tree))

def main(args: List[String]) =
  import PgnHelper.onlyMoves
  val inputFile = args
    .get(0)
    .getOrElse(throw new RuntimeException("no path to PGN file found"))
  val nbPgn      = args.get(1).map(_.toInt)
  val outputFile = args.get(2).getOrElse("theOne.pgn")
  val pgns       = Source.fromFile(inputFile).mkString.split("\n\n\n").map(parse).map(_.onlyMoves)
  if merge(pgns(0), pgns(0)) != pgns(0)
  then throw new RuntimeException("merge is not idempotent on first PGN!")
  val selectedPgns = pgns.take(nbPgn.getOrElse(pgns.length))
  val theOne       = selectedPgns.foldLeft(pgns(0))(merge(_, _))
  writeFile(outputFile, theOne.toPgn.render.value)

main(args.toList)