package wueortho.io.random

import wueortho.util.Codecs.given
import wueortho.data.*

import io.circe.derivation.*
import scala.util.Random

object RandomGraphs:
  case class RandomGraphConfig(n: Int, m: Int, seed: Seed, core: GraphCore, allowLoops: Boolean)

  enum GraphCore derives CanEqual, ConfiguredEnumCodec:
    case Empty, Path, Tree, Star

  def mkSimpleGraph(config: RandomGraphConfig): Either[String, BasicGraph] =
    import config.*

    def nodePair(rndm: Random): (NodeIndex, NodeIndex) =
      val (u, v) = rndm.nextInt(n) -> rndm.nextInt(n)
      if !allowLoops && u == v then nodePair(rndm)
      else NodeIndex(u) -> NodeIndex(v)

    def mkCore(rndm: Random): Seq[SimpleEdge] = core match
      case GraphCore.Empty => Nil
      case GraphCore.Path  =>
        if n < 2 then Nil
        else (for Seq(u, v) <- (NodeIndex(0) until n).sliding(2) yield SimpleEdge(u, v)).toSeq
      case GraphCore.Tree  =>
        if n < 1 then Nil
        else for i <- 1 until n yield SimpleEdge(NodeIndex(rndm.nextInt(i)), NodeIndex(i))
      case GraphCore.Star  =>
        if n < 1 then Nil
        else for i <- 1 until n yield SimpleEdge(NodeIndex(0), NodeIndex(i))

    def mkHull(rndm: Random, coreSize: Int): Either[String, Seq[SimpleEdge]] = Either.cond(
      coreSize <= m,
      for _ <- coreSize until m yield SimpleEdge.apply.tupled(nodePair(rndm)),
      s"Core size was ${coreSize} but only ${m} edges expected",
    )

    val rndm      = seed.newRandom
    val coreEdges = mkCore(rndm)
    mkHull(rndm, coreEdges.size).map(hullEdges => Graph.fromEdges(coreEdges ++ hullEdges).mkSimpleGraph)
  end mkSimpleGraph
end RandomGraphs
