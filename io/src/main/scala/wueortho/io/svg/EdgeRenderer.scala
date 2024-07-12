// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.io.svg

import wueortho.data.*
import wueortho.data.EdgeRoute.OrthoSeg, OrthoSeg.*

import scala.annotation.tailrec

object EdgeRenderer:
  def smoothSvgPathCommands(svg: Svg, route: EdgeRoute, bendRadius: Double) =
    val (tx, ty) = Svg.pixelTransformers(svg)
    val radius   = bendRadius / svg.pixelsPerUnit

    def seg(s: OrthoSeg, crop: Double = 2 * radius) = s match
      case HSeg(dx) => s"h ${tx(dx - Math.signum(dx) * crop)}"
      case VSeg(dy) => s"v ${ty(dy - Math.signum(dy) * crop)}"

    def bend(s: OrthoSeg, a: Double, b: Double) = s match
      case HSeg(_) => s"q ${tx(a)},0 ${tx(a)},${ty(b)}"
      case VSeg(_) => s"q 0,${ty(a)} ${tx(b)},${ty(a)}"

    def smooth(s: OrthoSeg, a: Double, b: Double) = s match
      case HSeg(_) => s"t ${tx(a)},${ty(b)}"
      case VSeg(_) => s"t ${tx(b)},${ty(a)}"

    @tailrec def go(res: List[String], a: OrthoSeg, l: List[OrthoSeg]): List[String] = l match
      case Nil       => res.reverse
      case b :: next =>
        val (aTooShort, bTooShort) = (a.len < 2 * radius, b.len < 2 * radius)
        if aTooShort then
          if bTooShort then go(smooth(a, a.sgn * a.len / 2, b.sgn * b.len / 2) :: res, b, next)
          else go(smooth(a, a.sgn * a.len / 2, b.sgn * radius) :: res, b, next)
        else if bTooShort then go(bend(a, a.sgn * radius, b.sgn * b.len / 2) :: seg(a) :: res, b, next)
        else go(bend(a, a.sgn * radius, b.sgn * radius) :: seg(a) :: res, b, next)

    val (s, t) = route.terminals.uTerm -> route.terminals.vTerm
    val pre    = s"M ${tx(s.x1)} ${ty(s.x2)}"
    val post   = s"L ${tx(t.x1)} ${ty(t.x2)}"

    val mid = route.route match
      case Seq()       => Seq.empty
      case Seq(one)    => Seq(seg(one, 0.0))
      case h +: g +: t =>
        if h.len < 2 * radius then
          go(bend(h, h.sgn * h.len / 2, g.sgn * (radius min (g.len / 2))) :: seg(h, h.len / 2) :: Nil, g, t.toList)
        else go(seg(h, h.len - radius) :: Nil, h, g :: t.toList)

    pre +: mid :+ post
  end smoothSvgPathCommands

  def straightSvgPathCommands(svg: Svg, route: EdgeRoute) =
    val (tx, ty) = Svg.pixelTransformers(svg)

    val s = route.terminals.uTerm
    val d = s"M ${tx(s.x1)} ${ty(s.x2)}"
    val m = route.route.map:
      case HSeg(dx) => s"h ${tx(dx)}"
      case VSeg(dy) => s"v ${ty(dy)}"
    d +: m
  end straightSvgPathCommands
end EdgeRenderer
