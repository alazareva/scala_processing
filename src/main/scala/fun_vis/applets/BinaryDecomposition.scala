package fun_vis.applets

import fun_vis.functions.Fractals
import fun_vis.utils.ComplexNumber
import fun_vis._
import processing.core.PApplet.map
import processing.core.{PApplet, PConstants}
import fun_vis.ColorUtils.selectColor


class BinaryDecomposition extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  val pCanvas = ProcessingCanvas(
    Canvas(700, 700),
    CustomExtent(2f, 2f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 500
    val c = ComplexNumber(0.15, 0)
    val f = (z: ComplexNumber) => z.pow(2) + c
    val colorFn = (p: Point) => {
      val iterations = Fractals.juliaIterationResult(ComplexNumber(p.x, p.y), f, maxIterations=maxIterations)
      selectColor(iterations.c.i > 0, Color.black, Color.white)
    }
    pCanvas.foreach(colorFn)
    saveFrame("binary-######.jpeg")
  }
}

object BinaryDecomposition extends PApplet {
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.BinaryDecomposition")
  }
}
