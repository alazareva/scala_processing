package fun_vis.applets


import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.scalePoint
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, Color, ColorMode, ColorUtils, Extent, HSB, HSBColor, ProcessingCanvas, Vector}
import fun_vis.functions.Fractals
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map


class Mandelbrot extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(500, 500) / 3,
    Extent(1.5f, 1.5f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 100
    val colorFunction: ColorImage = p => {
      val f = (c1: ComplexNumber, c2: ComplexNumber) => (c1 * c1) + c2
      val iterations = Fractals.mandelbrot(maxIterations=maxIterations)(ComplexNumber(p.x, p.y), f)
      val b = map(iterations, 0, maxIterations, 0, 100)
      selectColor(iterations % 2 == 0, HSBColor(0, 0, b), HSBColor(0, 0, 100 - b))
    }
    pCanvas.foreach(colorFunction)
  }
}

object Mandelbrot extends PApplet {

  val pink = HSBColor(330, 59, 100)

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.Mandelbrot")
  }
}