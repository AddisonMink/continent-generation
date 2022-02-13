package continent

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.HTMLCanvasElement

object Display {
  def prepareCanvas(
      id: String,
      width: Int,
      height: Int,
      pointSize: Int
  ): (Int, Int, String) => Unit = {
    val canvas = org.scalajs.dom.document
      .getElementById(id)
      .asInstanceOf[HTMLCanvasElement]

    canvas.width = width * pointSize
    canvas.height = height * pointSize
    canvas.style.backgroundColor = "black"

    val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    (x, y, color) => {
      context.fillStyle = color
      context.fillRect(x * pointSize, y * pointSize, pointSize, pointSize)
    }
  }
}
