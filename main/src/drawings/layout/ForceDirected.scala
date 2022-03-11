package drawings.layout

import drawings.data.{EdgeWeightedSimpleGraph, Vec2D, VertexLayout}

import scala.annotation.tailrec
import scala.util.Random

import java.lang.Math.sqrt

object ForceDirected:
  def layout(cfg: Config)(g: EdgeWeightedSimpleGraph, init: VertexLayout): VertexLayout =
    @tailrec
    def go(i: Int, temp: Double, pos: Vector[Vec2D]): Vector[Vec2D] =
      if i >= cfg.iterCap then pos
      else
        // repulsive forces:
        val afterRep =
          for v <- g.nodes yield
            (for u <- g.nodes if u != v yield
              val delta = pos(v) - pos(u) // todo ensure a gap
              delta.scale(cfg.repulsive(delta.len) / delta.len)
            ).fold(Vec2D(0, 0))(_ + _)

        // attractive forces:
        val disp = g.edges.foldLeft(afterRep.toVector)((d, edge) =>
          val delta = pos(edge.to) - pos(edge.from) // todo ensure non-zero length edge
          val force = delta.scale(cfg.attractive(delta.len) / delta.len * edge.weight)
          d.updated(edge.to, d(edge.to) - force).updated(edge.from, d(edge.from) + force)
        )

        val newPos = pos.zip(disp).map { case (p, d) =>
          p + (if d.len < temp then d else d.scale(temp / d.len))
        }

        go(i + 1, cfg.cooling(temp), newPos)
    end go

    VertexLayout(go(0, cfg.startingTemp, init.nodes.toVector))
  end layout

  case class Config(
    startingTemp: Double,
    iterCap: Int,
    cooling: Double => Double,
    repulsive: Double => Double,
    attractive: Double => Double,
  )

  val defaultConfig = Config(
    startingTemp = 10.0,
    iterCap = 100,
    cooling = x => (x - 0.1) * 0.99 + 0.1,
    repulsive = 1.0 / _,
    attractive = x => x * x,
  )

  def initLayout(rand: Random, n: Int) =
    VertexLayout(Vector.fill(n)(Vec2D(rand.nextGaussian() * sqrt(n), rand.nextGaussian() * sqrt(n))))

end ForceDirected
