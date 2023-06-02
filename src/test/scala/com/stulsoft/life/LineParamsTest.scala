/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life
import org.scalatest.flatspec.AnyFlatSpec
class LineParamsTest extends AnyFlatSpec:
  "LineParams" should "build array of points 45 degrees" in {
    val lp = LineParams(0, 0, 5, 5)
    val points = lp.points()
    assert(3 == points(2)._1)
    assert(3 == points(2)._2)
  }

  it should "build array of points -45 degrees" in {
    val lp = LineParams(5, 5, 0, 0)
    val points = lp.points()
    assert(2 == points(2)._1)
    assert(2 == points(2)._2)
  }

  it should "build array of points # 2" in {
    val lp = LineParams(0, 0, 2, 5)
    val points = lp.points()
    assert(1 == points(2)._1)
    assert(3 == points(2)._2)
  }
