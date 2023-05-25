package wueortho.layout

import wueortho.data.{WeightedGraph, Vec2D, VertexLayout}
import wueortho.util.WhenSyntax.*

import scala.annotation.tailrec
import scala.util.Random

import java.lang.Math.sqrt

object ForceDirected:
  private val EPS = 1e-8

  def layout(cfg: Config)(graph: WeightedGraph, init: VertexLayout): VertexLayout =

    class PosVec(init: Seq[Vec2D]):
      var a = Array[Vec2D](init*)
      var b = Array[Vec2D](init*)

      def apply(i: Int) = b(i)
      def delta(i: Int) = a(i) - b(i)
      def finish        = a.toVector

      def move(i: Int, delta: Vec2D)   = a(i) += delta
      def update(i: Int, value: Vec2D) = a(i) = value

      def applyChanges() = Array.copy(a, 0, b, 0, a.length)
    end PosVec

    @tailrec
    def go(i: Int, temp: Double, pos: PosVec): Vector[Vec2D] =
      if i < 0 then pos.finish
      else
        pos.applyChanges()
        val n = graph.numberOfVertices

        // repulsive forces:
        for
          v <- 0 until n
          u <- (v + 1) until n
        do
          val delta = (pos(v) - pos(u)) when (_.len > EPS) otherwise Vec2D(EPS, EPS)
          val disp  = delta.scale(cfg.repulsive(delta.len) / delta.len)
          pos.move(v, disp)
          pos.move(u, -disp)

        // attractive forces:
        for edge <- graph.edges if edge.from != edge.to do
          val delta = pos(edge.to.toInt) - pos(edge.from.toInt)
          if delta.len > EPS then
            val disp = delta.scale(cfg.attractive(delta.len) / delta.len * edge.weight)
            pos.move(edge.from.toInt, disp)
            pos.move(edge.to.toInt, -disp)

        // limit the displacement
        for i <- 0 until n do
          val d = pos.delta(i)
          if d.len > temp then pos(i) += d.scale(temp / d.len)

        go(i - 1, cfg.cooling(temp), pos)
    end go

    VertexLayout(go(cfg.iterCap, cfg.startingTemp, PosVec(init.nodes)))
  end layout

  case class Config(
      startingTemp: Double,
      iterCap: Int,
      cooling: Double => Double,
      repulsive: Double => Double,
      attractive: Double => Double,
  )

  val defaultConfig = Config(
    startingTemp = 15.0,
    iterCap = 1000,
    // cooling = x => (x - 0.1) * 0.995 + 0.1,
    cooling = x => (x - 0.02) max 0.1,
    repulsive = 1.0 / _,
    attractive = x => x * x,
  )

  def initLayout(rand: Random, n: Int) =
    VertexLayout(Vector.fill(n)(Vec2D(rand.nextGaussian() * sqrt(n), rand.nextGaussian() * sqrt(n))))

end ForceDirected
