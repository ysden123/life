/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import com.stulsoft.common.AppDataPath

import java.io.{File, FileReader, FileWriter}
import java.util.Properties

object Config:
  private val fileName = "application.properties"
  private val intervalName = "interval"
  private val defaultInterval = "500"
  private var interval: Int = defaultInterval.toInt

  load()

  private def appPropFile:File=
    new File(AppDataPath.appDataPath("life") + "\\" + fileName)

  private def load(): Unit =
    val properties = new Properties()
    try
      properties.load(new FileReader(appPropFile))
      interval = properties.getProperty(intervalName, defaultInterval).toInt
    catch
      case _: Exception =>
        println("Can't read application.properties")

  def getInterval: Int = interval

  def setInterval(interval: Int): Unit =
    this.interval = interval

  def save(): Unit =
    val properties = new Properties()
    properties.put(intervalName, interval.toString)
    try
      properties.store(new FileWriter(appPropFile), "")
    catch
      case exception: Exception =>
        println(s"Can't save properties: ${exception.getMessage}")