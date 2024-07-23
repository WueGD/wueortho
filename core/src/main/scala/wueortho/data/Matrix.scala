// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.data.mutable

import scala.collection.mutable

trait MatrixView[K]:
  def apply(col: Int, row: Int): K

  def max(using ev: Ordering[K]): K
  def min(using ev: Ordering[K]): K

  def show: String

private case class RowMajorDoubleMatrix(data: mutable.ArrayBuffer[Double], k: Int) extends MatrixView[Double]:
  override def apply(col: Int, row: Int)         = data(row * k + col)
  def update(col: Int, row: Int, newVal: Double) = data(row * k + col) = newVal

  override def max(using ev: Ordering[Double]): Double = data.max
  override def min(using ev: Ordering[Double]): Double = data.min

  override def show: String = data.grouped(k).map(_.mkString(", ")).mkString("\n")

object Matrix:
  def fill(numberOfColumns: Int, numberOfRows: Int)(value: => Double) =
    RowMajorDoubleMatrix(mutable.ArrayBuffer.fill(numberOfRows * numberOfColumns)(value), numberOfColumns)
