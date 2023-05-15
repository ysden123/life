/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

enum LifeStatus:
  case Live, Dead

class Life(val stageWidth: Int, val cellWidth: Int, val initialGeneration: Array[Array[LifeStatus]]) {
  require(stageWidth == initialGeneration.length)
  require(initialGeneration.forall(a => stageWidth == a.length))
  private var generation = initialGeneration.clone()
  private val archive: ArrayBuffer[Array[Array[LifeStatus]]] = ArrayBuffer.empty[Array[Array[LifeStatus]]]
  archive += initialGeneration
  private var generationNumber = 0
  private var liveCount = countLiveCells()
  private var reasonOfStop = ""

  def updateGeneration(): Int = {
    if archive.isEmpty then archive += generation
    reasonOfStop = ""
    generationNumber += 1
    if archive.size >= 10_000 then
      reasonOfStop = "More than 10 000 generations"
      liveCount = 0
      0
    else
      val nextGeneration = Array.ofDim[LifeStatus](stageWidth, stageWidth)
      for (x <- 0 until stageWidth) {
        for (y <- 0 until stageWidth) {
          val neighbors = findLiveNeighbors(x, y)
          var newStatus = generation(x)(y)
          if newStatus == LifeStatus.Dead then {
            // Previous status is dead
            if neighbors == 3 then
              newStatus = LifeStatus.Live
          } else {
            // Previous status is live
            if neighbors < 2 || neighbors > 3 then
              newStatus = LifeStatus.Dead
          }
          nextGeneration(x)(y) = newStatus
        }
      }

      // check, if generation exists in archive
      val alreadyExists = archive.exists(fromArchive => {
        var repeated = true
        for (x <- 0 until stageWidth) {
          for (y <- 0 until stageWidth) {
            if nextGeneration(x)(y) != fromArchive(x)(y) then
            // At least one cell is different
              repeated = false
          }
        }
        repeated
      })

      if alreadyExists then
        reasonOfStop = "A generation already existed"
        for (x <- 0 until stageWidth) {
          for (y <- 0 until stageWidth) {
            generation(x)(y) = LifeStatus.Dead
          }
        }
      else
        generation = nextGeneration.clone()
        archive += generation

      val totalLive = countLiveCells()
      liveCount = totalLive
      if totalLive == 0 && reasonOfStop.isEmpty then
        reasonOfStop = "No one live cell"
      totalLive
  }

  def updateStatus(x: Int, y: Int): Unit =
    generation(x)(y) = if defineLifeStatus(x, y) == LifeStatus.Live then LifeStatus.Dead else LifeStatus.Live

  def defineLifeStatus(x: Int, y: Int): LifeStatus =
    var xx = x
    var yy = y
    if x == -1 then xx = stageWidth - 1
    else if x == stageWidth then xx = 0

    if y == -1 then yy = stageWidth - 1
    else if y == stageWidth then yy = 0
    generation(xx)(yy)

  def findLiveNeighbors(x: Int, y: Int): Int = {
    var sum = 0
    if defineLifeStatus(x - 1, y - 1) == LifeStatus.Live then sum += 1
    if defineLifeStatus(x - 1, y) == LifeStatus.Live then sum += 1
    if defineLifeStatus(x - 1, y + 1) == LifeStatus.Live then sum += 1

    if defineLifeStatus(x, y - 1) == LifeStatus.Live then sum += 1
    if defineLifeStatus(x, y + 1) == LifeStatus.Live then sum += 1

    if defineLifeStatus(x + 1, y - 1) == LifeStatus.Live then sum += 1
    if defineLifeStatus(x + 1, y) == LifeStatus.Live then sum += 1
    if defineLifeStatus(x + 1, y + 1) == LifeStatus.Live then sum += 1
    sum
  }

  def printGeneration(): Unit =
    for (x <- 0 until stageWidth) {
      for (y <- 0 until stageWidth) {
        if generation(x)(y) == LifeStatus.Live then
          print("L")
        else
          print("D")
      }
      println("")
    }

  def regenerateInitialGeneration(): Unit =
    val random = Random
    archive.clear()

    for (_ <- 1 to 200) {
      val x = random.nextInt(stageWidth)
      val y = random.nextInt(stageWidth)
      generation(x)(y) = if random.nextBoolean() then
        LifeStatus.Live
      else
        LifeStatus.Dead
    }

    liveCount = countLiveCells()
    generationNumber = 0
    reasonOfStop = ""

  def saveInitialGeneration(): Unit =
    SaveGeneration.save(this, "test.txt", archive(0))

  def loadInitialGeneration(): Unit =
    archive.clear()
    val initialGenerationLoaded = SaveGeneration.load(this, "test.txt")
    archive += generation
    for (x <- 0 until stageWidth) {
      for (y <- 0 until stageWidth)
        initialGeneration(x)(y) = initialGenerationLoaded(x)(y)
        generation(x)(y) = initialGenerationLoaded(x)(y)
    }

    liveCount = countLiveCells()
    generationNumber = 0
    reasonOfStop = ""

  def reloadInitialGeneration(): Unit =
    if archive.nonEmpty then
      generation = archive(0).clone
    archive.clear()
    archive += generation

    liveCount = countLiveCells()
    generationNumber = 0
    reasonOfStop = ""

  def clearStage(): Unit =
    archive.clear()

    for (x <- 0 until stageWidth) {
      for (y <- 0 until stageWidth) {
        generation(x)(y) = LifeStatus.Dead
      }
    }
    liveCount = 0
    generationNumber = 0
    reasonOfStop = ""

  def buildStatusLineText(): String =
    s"Generation: $generationNumber, number of live cells: $liveCount. $reasonOfStop"

  private def countLiveCells(): Int =
    (for {
      x <- 0 until stageWidth
      y <- 0 until stageWidth if generation(x)(y) == LifeStatus.Live
    } yield 1).size
}
