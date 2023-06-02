/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

case class LineParams(x1: Int, y1: Int, x2: Int, y2: Int):
  require(x1 != x2 || y1 != y2)
  private val n = Math.max(Math.abs(x2 - x1), Math.abs(y2 - y1))
  private val kX = 1.0 * (x2 - x1) / n
  private val kY = 1.0 * (y2 - y1) / n

  override def toString: String =
    s"LineParams: x1=$x1, y1=$y1, x2=$x2, y2=$y2, n=$n, kX=$kX, kY=$kY, points=${points().toList}"

  def points(): Array[(Int, Int)] =
    val thePoints = Array.ofDim[(Int, Int)](n)
    for (i <- 1 to n) {
//      val x = Math.min(Math.round(x1 + i * kX).toInt, x2)
//      val y = Math.min(Math.round(y1 + i * kY).toInt, y2)
      val x = Math.round(x1 + i * kX).toInt
      val y = Math.round(y1 + i * kY).toInt
      thePoints(i - 1) = (x, y)
    }
    thePoints


