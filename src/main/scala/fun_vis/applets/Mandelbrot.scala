package fun_vis.applets


import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.scalePoint
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, Color, ColorMode, ColorUtils, CustomExtent, Extent, HSB, HSBColor, ProcessingCanvas, Vector}
import fun_vis.functions.Fractals
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map


class Mandelbrot extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  val pCanvas = ProcessingCanvas(
    Canvas(500, 500) / 3,
    CustomExtent(1.5f, 1.5f) + Vector(-0.6f, 0),
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
      val h = map(iterations, 0, maxIterations, 0, 20)
      selectColor(iterations % 2 == 0, HSBColor(120 + h, 50, b), HSBColor(200 + h, 50, 100 - b))
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