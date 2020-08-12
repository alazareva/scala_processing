package fun_vis.applets

import fun_vis.functions.Fractals
import fun_vis.utils.ComplexNumber
import fun_vis._
import processing.core.PApplet.map
import processing.core.{PApplet, PConstants}

class JuliaSet extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  val pCanvas = ProcessingCanvas(
    Canvas(700, 700) * 0.8f,
    (CustomExtent(1.5f, 1.5f) + Vector(-0.5f, -0.5f)) * 0.4f,
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 900
    val c = ComplexNumber(-0.1, 0.65)
    val f = (z: ComplexNumber) => z.pow(2) + c
    val colorFn = (p: Point) => {
      val iterations = Fractals.julia(ComplexNumber(p.x, p.y), f, maxIterations=maxIterations)
      val b = map(iterations, 0 , maxIterations, 0, 100)
      HSBColor(180, 90, b)
    }
    pCanvas.foreach(colorFn)
  }
}

object JuliaSet extends PApplet {
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.JuliaSet")
  }
}
