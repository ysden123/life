/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import com.stulsoft.common.AppDataPath
import com.typesafe.scalalogging.StrictLogging

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.swing.FileChooser
import scala.swing.FileChooser.Result.Approve

object SaveGeneration extends StrictLogging:
  def save(life: Life, initialGeneration: Array[Array[LifeStatus]]): Unit =
    val fileChooser = buildFileChooser()
    val choice = fileChooser.showSaveDialog(null)
    if choice == Approve then
      val file = fileChooser.selectedFile
      val printerWriter = new PrintWriter(file)
      for (y <- 0 until life.stageWidth) {
        val line = StringBuilder()
        for (x <- 0 until life.stageWidth) {
          line.append(if initialGeneration(x)(y) == LifeStatus.Live then "1" else "0")
        }
        printerWriter.println(line.toString())
      }
      printerWriter.close()

  def load(life: Life): Option[Array[Array[LifeStatus]]] =
    val fileChooser = buildFileChooser()
    val choice = fileChooser.showOpenDialog(null)
    if choice == Approve then
      val source = Source.fromFile(fileChooser.selectedFile)
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
      Option(initialGeneration)
    else
      Option.empty

  private def buildFileChooser(): FileChooser =
    val startDir = new File(AppDataPath.appDataPath("life"))
    new FileChooser(startDir)