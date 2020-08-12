package fun_vis.applets


import fun_vis.Types.ColorImage
import fun_vis.functions.Fractals
import fun_vis._
import processing.core.{PApplet, PConstants}

// Based on work by Paul Bourke http://paulbourke.net/fractals/mauldin/

class MauldinGasket extends PApplet {

  implicit val colorMode: ColorMode = HSB


  val pCanvas = ProcessingCanvas(
    Canvas(500, 500) / 5,
    CanvasExtent,
    this,
  )

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val image = Fractals.mauldinGasket(pCanvas.canvas.width, pCanvas.canvas.height)
    val maxLuma = image.max
    val logMax = Math.log(maxLuma + 1)

    val colorFunction: ColorImage = p => {
      val offset = pCanvas.canvas.width * p.y.toInt + p.x.toInt
      val s = (Math.log(image(offset) + 1)/logMax).toFloat * 60
      val b = (100 - Math.log(image(offset) + 1)/logMax).toFloat * 100
      HSBColor(340, s, b)
    }
    pCanvas.foreach(colorFunction)
  }
}

object MauldinGasket extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.MauldinGasket")
  }
}