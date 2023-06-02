/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import java.awt.Color
import scala.swing.event.{MouseClicked, MouseEvent, MousePressed}
import scala.swing.{Graphics2D, Panel}

class LifePanel(life: Life) extends Panel {

  override def paint(g: Graphics2D): Unit = {
    g.setColor(Color.BLACK)
    g.clearRect(0, 0, bounds.width, bounds.height)
    g.fillRect(10, 10, 10, 10)

    for (x <- 0 until life.stageWidth) {
      for (y <- 0 until life.stageWidth) {
        val color = if life.defineLifeStatus(x, y) == LifeStatus.Dead then Color.WHITE else Color.BLACK
        g.setColor(color)
        g.fillRect(x * life.cellWidth, y * life.cellWidth, life.cellWidth, life.cellWidth)
      }
    }
  }

  listenTo(mouse.clicks, mouse.moves, keys)
  reactions += {
    //    case MousePressed(_, p, _, _, _)(peer) =>
    case e: MousePressed =>
      val x = e.point.x / life.cellWidth
      val y = e.point.y / life.cellWidth
      if e.peer.isShiftDown then
        life.drawLine(x, y)
      else
        life.updateStatus(x, y)
      repaint()
      requestFocus()
  }
}
