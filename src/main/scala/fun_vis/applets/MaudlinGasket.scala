package fun_vis.applets


import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.scalePoint
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, CanvasExtent, Color, ColorMode, ColorUtils, Extent, HSB, HSBColor, ProcessingCanvas, Vector}
import fun_vis.functions.Fractals
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map


class MaudlinGasket extends PApplet {

  implicit val colorMode: ColorMode = HSB


  val pCanvas = ProcessingCanvas(
    Canvas(500, 500) / 3,
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
    val image: Array[Double] = Array.fill(pCanvas.canvas.width *  pCanvas.canvas.height){0}
    Fractals.maudlinGasket(image, pCanvas.canvas.width, pCanvas.canvas.height, maxIterations=10000000)
    val maxLuma = image.max
    val logMax = Math.log(maxLuma + 1)

    val colorFunction: ColorImage = p => {
      val offset = pCanvas.canvas.width * p.y.toInt + p.x.toInt

      HSBColor(120, 100, 100 * (Math.log(image(offset) + 1)/logMax).toFloat)
    }
    pCanvas.foreach(colorFunction)
  }
}

object MaudlinGasket extends PApplet {

  val pink = HSBColor(330, 59, 100)

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.MaudlinGasket")
  }
}