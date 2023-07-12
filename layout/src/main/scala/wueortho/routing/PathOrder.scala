package wueortho.routing

import wueortho.data.*
import Direction.*

import scala.collection.mutable

trait PathOrder:
  def topPaths(n: NodeIndex): Seq[Int]
  def rightPaths(n: NodeIndex): Seq[Int]

object PathOrder:
  extension (rg: RoutingGraph)
    private def linkDir(u: NodeIndex, v: NodeIndex) =
      rg.connection(u, v).getOrElse(sys.error(s"graph disconnected between $u and $v"))

  def apply(rg: RoutingGraph, paths: IndexedSeq[Path]) =
    val top   = mutable.ArrayBuffer.fill(rg.size)(mutable.ArrayBuffer.empty[Int])
    val right = mutable.ArrayBuffer.fill(rg.size)(mutable.ArrayBuffer.empty[Int])

    /*
     * T-property: a node never intermediate node of one path and terminal node of another
     * v1---v2 have a north or east edge
     * all north/east edges starting in a vertex < v1 are sorted
     * v1.east is sorted before v1.north
     */
    def mkLt(v1: NodeIndex, v2: NodeIndex)(pathIdA: Int, pathIdB: Int): Boolean =
      val startDir = rg.linkDir(v2, v1)
      val isSorted = if startDir == West then (i: Int) => i < v1.toInt else (i: Int) => i <= v1.toInt

      val (pa, pb)             = (paths(pathIdA).nodes, paths(pathIdB).nodes)
      val (i1a, i1b, i2a, i2b) = (pa.indexOf(v1), pb.indexOf(v1), pa.indexOf(v2), pb.indexOf(v2))
      assert(i1a >= 0 && i1b >= 0 && i2a >= 0 && i2b >= 0, s"could not find vertices $v1, $v2 in paths $pa and $pb")
      assert((i2a - i1a).abs == 1 && (i2b - i1b).abs == 1, s"not an edge #$v1--#$v2 on paths $pa and $pb")

      def go(a: Int, b: Int, dir: Direction, isReversed: Boolean): Boolean =
        // - a/b terminate in the same vertex -> violates T1
        // assert(da > 0 && a > 0 || da < 0 && a < pa.size - 1, s"T1 violation: a=$a in $pa and $pb (starting $v1--$v2)")
        // assert(db > 0 && b > 0 || db < 0 && b < pb.size - 1, s"T1 violation: b=$b in $pa and $pb (starting $v1--$v2)")
        val (da, db) = if isReversed then (i1a - i2a, i1b - i2b) else (i2a - i1a, i2b - i1b)

        if (a - da) < 0 || (a - da) >= pa.size || (b - db) < 0 || (b - db) >= pb.size then
          // - a/b terminate in the same vertex
          if isReversed then pathIdA < pathIdB
          else go(i1a, i1b, startDir.reverse, isReversed = true)
        else if pa(a - da) != pb(b - db) then
          // - a/b have no common next vertex: check their directions -> you are finished!
          val dirA = rg.linkDir(pa(a), pa(a - da))
          val dirB = rg.linkDir(pb(b), pb(b - db))
          dirOrder(dir, dirA) < dirOrder(dir, dirB)
        else
          val commonDir   = rg.linkDir(pa(a), pa(a - da))
          val maybeLookup = commonDir match
            case North => Option.when(isSorted(pa(a).toInt))(top(pa(a).toInt))
            case East  => Option.when(isSorted(pa(a).toInt))(right(pa(a).toInt))
            case South => Option.when(isSorted(pa(a - da).toInt))(top(pa(a - da).toInt))
            case West  => Option.when(isSorted(pa(a - da).toInt))(right(pa(a - da).toInt))
          twist(dir, commonDir) ^ maybeLookup.fold(go(a - da, b - db, commonDir, isReversed)): lut =>
            // extract order from c---a edge
            val (ia, ib) = lut.indexOf(pathIdA) -> lut.indexOf(pathIdB)
            assert(ia >= 0 && ib >= 0, s"lost track of paths #$pathIdA and #$pathIdB on node #${pa(a - da)}")
            ia < ib
        end if
      end go

      go(i1a, i1b, startDir, isReversed = false)
    end mkLt

    for // init
      (path, i) <- paths.zipWithIndex
      Seq(u, v) <- path.nodes.sliding(2)
    do
      rg.linkDir(u, v) match
        case North => top(u.toInt) += i
        case East  => right(u.toInt) += i
        case South => top(v.toInt) += i
        case West  => right(v.toInt) += i
    end for

    for // sort
      u        <- NodeIndex(0) until rg.size
      (v, isH) <- rg.neighbor(u, East).map(_ -> true) ++ rg.neighbor(u, North).map(_ -> false)
    do
      val ord = Ordering.fromLessThan(mkLt(u, v))
      if isH then right(u.toInt).sortInPlace()(using ord)
      else top(u.toInt).sortInPlace()(using ord)

    def checkNoSeparator[T](a: mutable.IndexedSeq[T], after: Int, b: mutable.IndexedSeq[T], until: Int) =
      !a.view.slice(after + 1, a.size).exists(b.view.slice(0, until).contains)

    for // knock-knees
      nodeId     <- NodeIndex(0) until rg.size
      fromLeft   <- rg.neighbor(nodeId, West).map(id => right(id.toInt))
      fromBottom <- rg.neighbor(nodeId, South).map(id => top(id.toInt))
    do
      val (toTop, toRight) = top(nodeId.toInt) -> right(nodeId.toInt)
      if toTop.nonEmpty && toRight.nonEmpty && fromLeft.nonEmpty && fromBottom.nonEmpty then
        val (wsh, neh) = fromBottom.lastIndexWhere(fromLeft.contains) -> toTop.indexWhere(toRight.contains)
        val (wsv, nev) = fromLeft.lastIndexWhere(fromBottom.contains) -> toRight.indexWhere(toTop.contains)
        val (wnh, seh) = toTop.lastIndexWhere(fromLeft.contains)      -> fromBottom.indexWhere(toRight.contains)
        val (wnv, sev) = fromLeft.indexWhere(toTop.contains)          -> toRight.lastIndexWhere(fromBottom.contains)
        if wsh >= 0 && neh >= 0 && checkNoSeparator(fromBottom, wsh, toTop, neh) then
          fromBottom.insert(wsh + 1, toTop(neh))
          toTop.insert(neh, fromBottom(wsh))
        if wsv >= 0 && nev >= 0 && checkNoSeparator(fromLeft, wsv, toRight, nev) then
          fromLeft.insert(wsv + 1, toRight(nev))
          toRight.insert(nev, fromLeft(wsv))
        if wnh >= 0 && seh >= 0 && checkNoSeparator(toTop, wnh, fromBottom, seh) then
          toTop.insert(wnh + 1, fromBottom(seh))
          fromBottom.insert(seh, toTop(wnh))
        if wnv >= 0 && sev >= 0 && checkNoSeparator(toRight, sev, fromLeft, wnv) then
          fromLeft.insert(wnv, toRight(sev))
          toRight.insert(sev + 1, fromLeft(wnv))
      end if
    end for

    new RoutingGraph with PathOrder:
      val (t, r) = (top.map(_.toSeq), right.map(_.toSeq))

      export rg.*
      override def topPaths(n: NodeIndex)   = t(n.toInt)
      override def rightPaths(n: NodeIndex) = r(n.toInt)
  end apply

  // we sort south to north and west to east

  private def dirOrder(cur: Direction, next: Direction) = next match
    case North => if cur.isHorizontal then 1 else 0
    case East  => if cur.isHorizontal then 0 else 1
    case South => if cur.isHorizontal then -1 else 0
    case West  => if cur.isHorizontal then 0 else -1

  private def twist(cur: Direction, next: Direction) = PartialFunction.cond(cur -> next):
    case (North, East) | (East, North) | (South, West) | (West, South) => true

end PathOrder
