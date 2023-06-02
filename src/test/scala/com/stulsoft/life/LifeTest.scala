/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import org.scalatest.flatspec.AnyFlatSpec

class LifeTest extends AnyFlatSpec:
  "Life" should "validate initialGeneration" in {
    val ig = Array.ofDim[LifeStatus](2, 2)
    ig(0)(0) = LifeStatus.Dead
    ig(0)(1) = LifeStatus.Live
    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Live
    val l = Life(2, 2, ig)
    assert(null != l)
  }

  it should "throw exception in case wrong initialGeneration" in {
    assertThrows[IllegalArgumentException] {
      val ig = Array.ofDim[LifeStatus](1, 2)
      ig(0)(0) = LifeStatus.Dead
      ig(0)(1) = LifeStatus.Live
      Life(3, 2, ig)
    }
    assertThrows[IllegalArgumentException] {
      val ig = Array.ofDim[LifeStatus](1, 2)
      ig(0)(0) = LifeStatus.Dead
      ig(0)(1) = LifeStatus.Live
      Life(1, 2, ig)
    }
    assertThrows[IllegalArgumentException] {
      val ig = Array.ofDim[LifeStatus](2, 2)
      ig(0)(0) = LifeStatus.Dead
      ig(0)(1) = LifeStatus.Live
      ig(1)(0) = LifeStatus.Dead
      ig(1)(1) = LifeStatus.Live
      Life(3, 2, ig)
    }
  }

  it should "findLiveNeighbors" in {
    var ig = Array.ofDim[LifeStatus](2, 2)
    ig(0)(0) = LifeStatus.Dead
    ig(0)(1) = LifeStatus.Live
    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Live
    var l = Life(2, 2, ig)
    assert(l.findLiveNeighbors(0, 0) == 6)

    ig = Array.ofDim[LifeStatus](3, 3)
    ig(0)(0) = LifeStatus.Dead
    ig(0)(1) = LifeStatus.Dead
    ig(0)(2) = LifeStatus.Dead
    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Live
    ig(1)(2) = LifeStatus.Dead
    ig(2)(0) = LifeStatus.Dead
    ig(2)(1) = LifeStatus.Dead
    ig(2)(2) = LifeStatus.Dead
    l = Life(3, 2, ig)
    assert(l.findLiveNeighbors(1, 1) == 0)

    ig = Array.ofDim[LifeStatus](3, 3)
    ig(0)(0) = LifeStatus.Dead
    ig(0)(1) = LifeStatus.Dead
    ig(0)(2) = LifeStatus.Dead
    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Live
    ig(1)(2) = LifeStatus.Live
    ig(2)(0) = LifeStatus.Dead
    ig(2)(1) = LifeStatus.Dead
    ig(2)(2) = LifeStatus.Dead
    l = Life(3, 2, ig)
    assert(l.findLiveNeighbors(1, 1) == 1)
  }

  it should "updateGeneration - return 0" in {
    val ig = Array.ofDim[LifeStatus](3, 3)
    ig(0)(0) = LifeStatus.Dead
    ig(0)(1) = LifeStatus.Dead
    ig(0)(2) = LifeStatus.Dead

    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Live
    ig(1)(2) = LifeStatus.Dead

    ig(2)(0) = LifeStatus.Dead
    ig(2)(1) = LifeStatus.Dead
    ig(2)(2) = LifeStatus.Dead

    val l = Life(3, 2, ig)
    assert(l.updateGeneration() == 0)
  }

  it should "updateGeneration - return not 0" in {
    val ig = Array.ofDim[LifeStatus](3, 3)
    ig(0)(0) = LifeStatus.Live
    ig(0)(1) = LifeStatus.Dead
    ig(0)(2) = LifeStatus.Live

    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Dead
    ig(1)(2) = LifeStatus.Dead

    ig(2)(0) = LifeStatus.Live
    ig(2)(1) = LifeStatus.Dead
    ig(2)(2) = LifeStatus.Dead

    val l = Life(3, 2, ig)
    println("Initial generation:")
    l.printGeneration()

    val n= l.findLiveNeighbors(1,1)
    assert(n == 3)
    assert(l.updateGeneration() == 9)
    println("Updated generation 1:")
    l.printGeneration()

    assert(l.updateGeneration() == 0)
    println("Updated generation 2:")
    l.printGeneration()
  }

  it should "build few generations" in {
    val ig = Array.ofDim[LifeStatus](4, 4)
    ig(0)(0) = LifeStatus.Live
    ig(0)(1) = LifeStatus.Dead
    ig(0)(2) = LifeStatus.Live
    ig(0)(3) = LifeStatus.Dead

    ig(1)(0) = LifeStatus.Dead
    ig(1)(1) = LifeStatus.Dead
    ig(1)(2) = LifeStatus.Dead
    ig(1)(3) = LifeStatus.Dead

    ig(2)(0) = LifeStatus.Live
    ig(2)(1) = LifeStatus.Dead
    ig(2)(2) = LifeStatus.Dead
    ig(2)(3) = LifeStatus.Dead

    ig(3)(0) = LifeStatus.Dead
    ig(3)(1) = LifeStatus.Dead
    ig(3)(2) = LifeStatus.Dead
    ig(3)(3) = LifeStatus.Dead

    val l = Life(4, 2, ig)
    println("Initial generation:")
    l.printGeneration()

    if l.updateGeneration() > 0 then
      println("Generation 1:")
      l.printGeneration()

    if l.updateGeneration() > 0 then
      fail("Must be only 1 generation after initial")
  }

  it should "support line drawing" in {
    val ig = Array.ofDim[LifeStatus](6, 6)
    val life=Life(6, 1, ig)
    life.clearStage()
    life.drawLine(5,1)
    life.clearStage()
    life.drawLine(4,3)
  }