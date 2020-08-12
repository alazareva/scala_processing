package fun_vis.applets

import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.scalePoint
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, Color, ColorMode, ColorUtils, CustomExtent, Extent, HSB, HSBColor, ProcessingCanvas, Vector}
import fun_vis.functions.ColorFunctions.grayToHSB
import fun_vis.functions.Fractals
import fun_vis.functions.GrayFunctions.{angle, polarDist}
import fun_vis.functions.MaskFunctions.{altRingsOf, checkerOf}
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}


class RainbowMandelbrot extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(800, 500),
    CustomExtent(-3, 2, -1.5f, 1.5f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 100
    val colorFunction: ColorImage = p => {
      val scaled = scalePoint(Vector(0.7f, 0.7f))(p)
      val f = (c1: ComplexNumber, c2: ComplexNumber) => (c1 * c1) + c2
      val iterations = Fractals.mandelbrot(maxIterations=maxIterations, escape=10)(ComplexNumber(scaled.x, scaled.y), f)
      val scalePolarDistance = (v: Value) => v * 300.0f + frameCount * 20
      val rainbow = grayToHSB(polarDist andThen scalePolarDistance)(p)
      val mask = xorMask(checkerOf(angle(p)))(altRingsOf(frameCount % 50 + 2))
      ColorUtils.selectColor(
        mask(scaled),
        selectColor(iterations == maxIterations, Color.black, RainbowMandelbrot.pink),
        selectColor(iterations == maxIterations, Color.white, rainbow)
      )
    }
    pCanvas.foreach(colorFunction)
  }
}

object RainbowMandelbrot extends PApplet {

  val pink = HSBColor(330, 59, 100)

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.RainbowMandelbrot")
  }
}