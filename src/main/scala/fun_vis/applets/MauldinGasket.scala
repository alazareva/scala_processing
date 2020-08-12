package fun_vis.applets


import fun_vis.Types.ColorImage
import fun_vis.functions.Fractals
import fun_vis._
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map


// Based on work by Paul Bourke http://paulbourke.net/fractals/mauldin/

class MauldinGasket extends PApplet {

  implicit val colorMode: ColorMode = HSB


  val pCanvas = ProcessingCanvas(
    Canvas(500, 500) * 2,
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
      val v = (Math.log(image(offset) + 1)/logMax).toFloat
      val s = v *  70
      val b = (100 - v) * 100
      val h = map(p.x.toFloat, 0, pCanvas.canvas.width, 360, 0)
      HSBColor(h, s, b)
    }
    pCanvas.foreach(colorFunction)
  }
}

object MauldinGasket extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.MauldinGasket")
  }
}