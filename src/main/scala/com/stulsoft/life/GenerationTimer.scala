/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

case class GenerationTimer(interval: Int, op: () => Unit, repeats: Boolean = true):
  private val timeOut = new javax.swing.AbstractAction() {
    def actionPerformed(e: java.awt.event.ActionEvent): Unit = op()
  }
  private var timer: javax.swing.Timer = new javax.swing.Timer(interval, timeOut)
  timer.setRepeats(repeats)

  def start(): Unit =
    timer.start()

  def stop(): Unit =
    timer.stop()

