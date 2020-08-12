package fun_vis.applets

import fun_vis.functions.Fractals
import fun_vis.{Canvas, ColorMode, CustomExtent, Extent, HSB, HSBColor, Point, PointUtils, ProcessingCanvas, Vector}
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.{map, radians}

class PhoenixFractals extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  val pCanvas = ProcessingCanvas(
    Canvas(1200, 700),
    CustomExtent(-1.2f * 0.8f, 1.2f * 0.8f, -1.0f* 0.7f, 0.6f * 0.7f) * 0.08f + Vector(-0.25f, -0.2f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 100
    val c = 0.56667
    val p = -0.5
    val f = (c1: ComplexNumber, c2: ComplexNumber) => c1.pow(2) + c + (c2 * p)
    val colorFn = (p1: Point) => {
      val p = PointUtils.rotatePoint(radians(90))(p1)
      val iterations = Fractals.phoenix(ComplexNumber(p.x, p.y), f, maxIterations=maxIterations)
      val b = map(iterations, 0 , maxIterations, 0, 100)
      if (iterations % 2 == 0) HSBColor(180, 90, b) else HSBColor(330, 60, b)
    }
    pCanvas.foreach(colorFn)
  }
}

object PhoenixFractals extends PApplet {
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.PhoenixFractals")
  }
}
