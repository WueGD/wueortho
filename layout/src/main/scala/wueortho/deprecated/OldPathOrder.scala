package wueortho.deprecated

import wueortho.data.*
import wueortho.routing.RoutingGraph

import scala.collection.mutable

type TopOrRight   = Direction.North.type | Direction.East.type
type LeftOrBottom = Direction.West.type | Direction.South.type

case class PathsOnGridNode(toTop: List[Int], toRight: List[Int]):
  def modify(dir: TopOrRight)(f: List[Int] => List[Int]) = dir match
    case Direction.North => copy(toTop = f(toTop))
    case Direction.East  => copy(toRight = f(toRight))

  def prepended(dir: TopOrRight, i: Int)                      = modify(dir)(i :: _)
  def insertWhere(dir: TopOrRight, i: Int)(p: Int => Boolean) = modify(dir) { l =>
    val at = l.indexWhere(p(_))
    if at == -1 then l :+ i else l.take(at) ::: i :: l.drop(at)
  }

object PathOrder:
  private def ifTopOrRight[R, R1 <: R, R2 <: R](dir: Direction)(f1: TopOrRight => R1)(f2: LeftOrBottom => R2): R =
    dir match
      case Direction.North => f1(Direction.North)
      case Direction.East  => f1(Direction.East)
      case Direction.South => f2(Direction.South)
      case Direction.West  => f2(Direction.West)

  private def reverseDir(dir: LeftOrBottom): TopOrRight = dir match
    case Direction.West  => Direction.East
    case Direction.South => Direction.North

  def apply($ : RoutingGraph, ports: PortLayout, paths: IndexedSeq[Path]) =
    val onGrid = mutable.ArrayBuffer.fill($.size)(PathsOnGridNode(Nil, Nil))

    def leftOnGrid(u: NodeIndex)   = $.neighbor(u, Direction.West).toList.flatMap(n => onGrid(n.toInt).toRight)
    def bottomOnGrid(u: NodeIndex) = $.neighbor(u, Direction.South).toList.flatMap(n => onGrid(n.toInt).toTop)

    def otherPathsOrder(u: NodeIndex, mainDir: Direction) = mainDir match
      case Direction.East  => bottomOnGrid(u).reverse ::: leftOnGrid(u) ::: onGrid(u.toInt).toTop
      case Direction.West  => bottomOnGrid(u) ::: onGrid(u.toInt).toRight ::: onGrid(u.toInt).toTop.reverse
      case Direction.North => leftOnGrid(u).reverse ::: bottomOnGrid(u) ::: onGrid(u.toInt).toRight
      case Direction.South => leftOnGrid(u) ::: onGrid(u.toInt).toTop ::: onGrid(u.toInt).toRight.reverse

    for
      (path, i) <- paths.zipWithIndex
      Seq(u, v) <- path.nodes.sliding(2)
    do
      $.portId(u) match // a port should have only one path
        case Some(portId) =>
          val mainDir = ports.portDir(portId)
          ifTopOrRight(mainDir) { tr =>
            onGrid(u.toInt) = onGrid(u.toInt).prepended(tr, i)
          } { lb =>
            onGrid(v.toInt) = onGrid(v.toInt).prepended(reverseDir(lb), i)
          }
        case None         =>
          val mainDir = $.connection(u, v).getOrElse(sys.error(s"path disconnected between $u and $v"))
          val others  = otherPathsOrder(u, mainDir)
          val preIdx  = others.indexOf(i)
          assert(preIdx > -1, s"segment $u -> $v should not be the start of a path")
          ifTopOrRight(mainDir) { tr =>
            onGrid(u.toInt) = onGrid(u.toInt).insertWhere(tr, i)(j => !(others.indexOf(j) < preIdx))
          } { lb =>
            onGrid(v.toInt) = onGrid(v.toInt).insertWhere(reverseDir(lb), i)(j => !(others.indexOf(j) < preIdx))
          }
    end for

    onGrid.toIndexedSeq
  end apply

end PathOrder
