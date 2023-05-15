/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import com.typesafe.scalalogging.StrictLogging

import java.io.{File, FileReader, FileWriter, PrintWriter}
import scala.io.Source

object SaveGeneration extends StrictLogging:
  def save(life: Life, name: String, initialGeneration: Array[Array[LifeStatus]]): Unit =
    val file = new File(name)
    val printerWriter = new PrintWriter(file)
    for (y <- 0 until life.stageWidth) {
      val line = StringBuilder()
      for (x <- 0 until life.stageWidth) {
        line.append(if initialGeneration(x)(y) == LifeStatus.Live then "1" else "0")
      }
      printerWriter.println(line.toString())
    }
    printerWriter.close()

  def load(life: Life, name: String): Array[Array[LifeStatus]] =
    val source = Source.fromFile(name)
    val initialGeneration = Array.ofDim[LifeStatus](life.stageWidth, life.stageWidth)
    life.clearStage()
    val lines = source.getLines()
    for (y <- 0 until life.stageWidth) {
      val line = lines.next()
      for (x <- 0 until life.stageWidth) {
        val status = if line(x) == '1' then LifeStatus.Live else LifeStatus.Dead
        initialGeneration(x)(y) = status
      }
    }
    source.close()
    initialGeneration
