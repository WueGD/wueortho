package drawings.routing

import drawings.data.*
import scala.collection.mutable

import Direction.*

trait PathOrder:
  def topPaths(n: NodeIndex): Seq[Int]
  def rightPaths(n: NodeIndex): Seq[Int]

object PathOrder:
  def apply(rg: RoutingGraph, paths: IndexedSeq[Path]) =
    val top   = mutable.ArrayBuffer.fill(rg.size)(mutable.ArrayBuffer.empty[Int])
    val right = mutable.ArrayBuffer.fill(rg.size)(mutable.ArrayBuffer.empty[Int])

    /*
     * T1-property: all paths end in a degree 1 vertex
     * v1---v2 have a north or east edge
     * all north/east edges starting in a vertex < v1 are sorted
     * v1.east is sorted before v1.north
     */
    def mkLt(v1: NodeIndex, v2: NodeIndex)(pathIdA: Int, pathIdB: Int): Boolean =
      val startDir = rg.connection(v2, v1).getOrElse(sys.error(s"graph disconnected between $v1 and $v2"))
      val isSorted = if startDir == West then (i: Int) => i < v1.toInt else (i: Int) => i <= v1.toInt

      val (pa, pb)             = (paths(pathIdA).nodes, paths(pathIdB).nodes)
      val (i1a, i1b, i2a, i2b) = (pa.indexOf(v1), pb.indexOf(v1), pa.indexOf(v2), pb.indexOf(v2))
      assert(i1a > 0 && i1b > 0 && i2a > 0 && i2b > 0, s"could not find vertices #$v1, #$v2 in paths $pa and $pb")
      val (da, db)             = (i2a - i1a, i2b - i1b)
      assert((da == 1 || da == -1) && (db == 1 || db == -1), s"not an edge #$v1--#$v2 on paths $pa and $pb")

      def go(a: Int, b: Int, dir: Direction): Boolean =
        // - a/b terminate in the same vertex -> violates T1
        assert(da > 0 && a > 0 || da < 0 && a < pa.size - 1, s"T1 violation: a=$a in $pa and $pb (starting $v1--$v2)")
        assert(db > 0 && b > 0 || db < 0 && b < pb.size - 1, s"T1 violation: b=$b in $pa and $pb (starting $v1--$v2)")

        if pa(a - da) != pb(b - db) then
          // - a/b have no common next vetex towards left/down: check their directions -> you are finished!
          (for
            dirA <- rg.connection(pa(a), pa(a - da))
            dirB <- rg.connection(pb(b), pb(b - db))
          yield dirOrder(dir, dirA) < dirOrder(dir, dirB))
            .getOrElse(sys.error(s"path $pa disconnected at #$a || path $pb disconnected at #$b"))
        else
          val commonDir   = rg.connection(pa(a), pa(a - da)).getOrElse(sys.error(s"path $pa disconnected at #$a"))
          val maybeLookup = commonDir match
            case North => Option.when(isSorted(pa(a).toInt))(top(pa(a).toInt))
            case East  => Option.when(isSorted(pa(a).toInt))(right(pa(a).toInt))
            case South => Option.when(isSorted(pa(a - da).toInt))(top(pa(a - da).toInt))
            case West  => Option.when(isSorted(pa(a - da).toInt))(right(pa(a - da).toInt))
          twist(dir, commonDir) ^ maybeLookup.fold(go(a - da, b - db, commonDir))(lut =>
            // extract order from c---a edge
            val (ia, ib) = lut.indexOf(pathIdA) -> lut.indexOf(pathIdB)
            assert(ia >= 0 && ib >= 0, s"lost track of paths #$pathIdA and #$pathIdB on node #${pa(a - da)}")
            ia < ib,
          )
      end go

      go(i1a, i1b, startDir)
    end mkLt

    for
      (path, i) <- paths.zipWithIndex
      Seq(u, v) <- path.nodes.sliding(2)
      dir       <- rg.connection(u, v).orElse(sys.error(s"path disconnected between $u and $v"))
    do
      dir match
        case North => top(u.toInt) += i
        case East  => right(u.toInt) += i
        case South => top(v.toInt) += i
        case West  => right(v.toInt) += i
    end for

    for
      u        <- NodeIndex(0) until rg.size
      (v, isH) <- rg.neighbor(u, East).map(_ -> true) ++ rg.neighbor(u, North).map(_ -> false)
    do
      val ord = Ordering.fromLessThan(mkLt(u, v))
      if isH then right(u.toInt).sortInPlace()(ord)
      else top(u.toInt).sortInPlace()(ord)

    new RoutingGraph with PathOrder:
      export rg.*
      override def topPaths(n: NodeIndex)   = top(n.toInt).toSeq
      override def rightPaths(n: NodeIndex) = right(n.toInt).toSeq
  end apply

  // we sort south to north and west to east

  private def dirOrder(cur: Direction, next: Direction) = next match
    case North => if cur.isHorizontal then 1 else 0
    case East  => if cur.isHorizontal then 0 else 1
    case South => if cur.isHorizontal then -1 else 0
    case West  => if cur.isHorizontal then 0 else -1

  private def twist(cur: Direction, next: Direction) = cur -> next match
    case (North, East) | (East, North) | (South, West) | (West, South) => true
    case _                                                             => false

end PathOrder
