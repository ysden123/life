/*
 * Copyright (c) 2023. StulSoft
 */

package com.stulsoft.life

import scala.swing.*
import scala.swing.BorderPanel.Position
import scala.swing.event.ButtonClicked

object ConfigDialog {
  def showDialog(mainFrame: MainFrame): Unit =
    new Dialog(mainFrame) {
      val dialogFrame: Dialog = this
      title = "Configuration"
      size = new Dimension(300, 300)
      resizable = false
      centerOnScreen()
      val intervalField = new TextField(Config.getInterval.toString)
      val okButton: Button = new Button("OK"){
        reactions += {
          case ButtonClicked(_) =>
            try
              Config.setInterval(intervalField.text.toInt)
            catch
              case _: Exception => println(s"Wrong interval value")
            Config.save()
            dialogFrame.close()
        }
      }
      val cancelButton: Button = new Button("Cancel"){
          reactions += {
            case ButtonClicked(_) =>
              println("cancel")
              dialogFrame.close()
          }
      }

      val dataPanel: GridPanel = new GridPanel(1, 2) {
        contents ++= Seq(new Label("interval"), intervalField)
      }

      val buttonPanel: GridPanel = new GridPanel(2,2){
        contents ++= Seq(new Label(""), new Label(""), okButton, cancelButton)
      }
      contents = new BorderPanel{
        layout(dataPanel) = Position.Center
        layout(buttonPanel) = Position.South
      }
    }
      .open()
}
