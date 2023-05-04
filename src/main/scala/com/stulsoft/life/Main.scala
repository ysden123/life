/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import scala.swing.{Action, Dimension, Frame, MainFrame, Menu, MenuBar, MenuItem, SimpleSwingApplication}
import scala.util.Random

object Main extends SimpleSwingApplication:
  override def top: Frame = new MainFrame {
    title = "Life, automata"
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

    val life: Life = Life(stageWidth, cellWidth, initialGeneration)
    val lifePanel = new LifePanel(life)
    contents = lifePanel

    val timer: GenerationTimer = GenerationTimer(500, tick)

    def tick(): Unit =
      val liveCount = life.updateGeneration()
      lifePanel.repaint()
      if liveCount == 0 then
        timer.stop()

    menuBar = new MenuBar {
      contents += new Menu("Generation") {
        contents += new MenuItem(Action("Clear stage") {
          life.clearStage()
          lifePanel.repaint()
        })
        contents += new MenuItem(Action("Regenerate initial generation") {
          life.regenerateInitialGeneration()
          lifePanel.repaint()
        })

        contents += new MenuItem(Action("Next generation") {
          val liveCount = life.updateGeneration()
          lifePanel.repaint()
        })

        contents += MenuItem(Action("Start") {
          timer.start()
        })

        contents += MenuItem(Action("Stop") {
          timer.stop()
        })
      }
    }

    size = new Dimension(600, 600)
    centerOnScreen()
  }

  private def generateInitialGeneration(initialGeneration: Array[Array[LifeStatus]], stageWidth: Int): Unit =
    val random = Random

    for(_ <- 1 to 200){
      val x = random.nextInt(stageWidth)
      val y = random.nextInt(stageWidth)
      initialGeneration(x)(y) = if random.nextBoolean() then LifeStatus.Live else LifeStatus.Dead
    }