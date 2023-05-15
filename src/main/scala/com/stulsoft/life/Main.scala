/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import com.sun.java.swing.ui.StatusBar

import scala.swing.BorderPanel.Position
import scala.swing.{Action, BorderPanel, Dimension, Frame, Label, MainFrame, Menu, MenuBar, MenuItem, SimpleSwingApplication}
import scala.util.Random

object Main extends SimpleSwingApplication:
  override def top: Frame = new MainFrame {
    val theMainFrame: MainFrame = this
    val version: String = ManifestInfo("com.stulsoft", "life").version() match
      case Some(version) =>
        version
      case None =>
        ""
    title = "Life, automata " + version
    this.resizable = false
    val cellWidth = 10
    val stageWidth: Int = 600 / cellWidth
    val initialGeneration: Array[Array[LifeStatus]] = Array.fill[LifeStatus](stageWidth, stageWidth) {
      LifeStatus.Dead
    }

    generateInitialGeneration(initialGeneration, stageWidth)
    /*
        initialGeneration(10)(10) = LifeStatus.Live
        initialGeneration(10)(11) = LifeStatus.Live
        initialGeneration(11)(10) = LifeStatus.Live
        initialGeneration(30)(10) = LifeStatus.Live
        initialGeneration(10)(30) = LifeStatus.Live
    */

    val statusLine = new Label(" ")
    val life: Life = Life(stageWidth, cellWidth, initialGeneration)
    val lifePanel = new LifePanel(life)
    updateStatusLine(statusLine, life)
    contents = new BorderPanel {
      layout(statusLine) = Position.South
      layout(lifePanel) = Position.Center
    }

    var timer: GenerationTimer = _

    def tick(): Unit =
      val liveCount = life.updateGeneration()
      lifePanel.repaint()
      updateStatusLine(statusLine, life)
      if liveCount == 0 then
        timer.stop()

    menuBar = new MenuBar {
      contents += new Menu("Configuration") {
        contents += new MenuItem(Action("Change configuration") {
          ConfigDialog.showDialog(theMainFrame)
        })
      }
      contents += new Menu("Generation") {
        contents += new MenuItem(Action("Clear stage") {
          life.clearStage()
          lifePanel.repaint()
          updateStatusLine(statusLine, life)
        })
        contents += new MenuItem(Action("Regenerate initial generation") {
          life.regenerateInitialGeneration()
          lifePanel.repaint()
          updateStatusLine(statusLine, life)
        })

        contents += new MenuItem(Action("Next generation") {
          life.updateGeneration()
          lifePanel.repaint()
          updateStatusLine(statusLine, life)
        })

        contents += MenuItem(Action("Start") {
          timer = GenerationTimer(Config.getInterval, tick)
          timer.start()
        })

        contents += MenuItem(Action("Stop") {
          if timer != null then
            timer.stop()
        })

        contents += MenuItem(Action("Reload initial generation") {
          life.reloadInitialGeneration()
          lifePanel.repaint()
          updateStatusLine(statusLine, life)
        })

        contents += MenuItem(Action("Save initial generation") {
          life.saveInitialGeneration()
        })

        contents += MenuItem(Action("Load initial generation") {
          life.loadInitialGeneration()
          lifePanel.repaint()
          updateStatusLine(statusLine, life)
        })
      }
    }

    lifePanel.requestFocus()
    size = new Dimension(600 + 2, 600 + 16)
    centerOnScreen()
  }

  private def generateInitialGeneration(initialGeneration: Array[Array[LifeStatus]], stageWidth: Int): Unit =
    val random = Random

    for (_ <- 1 to 200) {
      val x = random.nextInt(stageWidth)
      val y = random.nextInt(stageWidth)
      initialGeneration(x)(y) = if random.nextBoolean() then LifeStatus.Live else LifeStatus.Dead
    }

  private def updateStatusLine(sl: Label, life: Life): Unit =
    sl.text = life.buildStatusLineText()