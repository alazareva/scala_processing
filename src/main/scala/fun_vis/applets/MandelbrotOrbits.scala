package fun_vis.applets


import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.{complexNumberToPoint, pointToComplexNumber}
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, Color, ColorMode, ColorUtils, CustomExtent, Extent, HSB, HSBColor, ProcessingCanvas, Vector}
import fun_vis.functions.Fractals
import fun_vis.functions.SpatialTransforms
import fun_vis.functions.GrayFunctions.angle
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map


class MandelbrotOrbits extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
    noLoop()
  }

  val pCanvas = ProcessingCanvas(
    Canvas(700, 700) * 2,
    CustomExtent(1.5f, 1.5f) + Vector(-0.6f, 0),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 1000
    val orbitMax = 1500
    val hue = 240
    val colorFunction: ColorImage = p => {
      val f = (c1: ComplexNumber, c2: ComplexNumber) => (c1 * c1) + c2
      val (escaped, orbit) = Fractals.mandelbrotOrbitSize(maxIterations=maxIterations)(pointToComplexNumber(p), f)
      if (escaped) HSBColor(hue, 50, map(orbit, 0, 100, 0, 360))
      else HSBColor(hue, 50, map(orbit, 0, orbitMax, 40, 80))
    }
    pCanvas.foreach(colorFunction)
    saveFrame("orbits_blue.jpeg")
  }
}

object MandelbrotOrbits extends PApplet {

  val pink = HSBColor(330, 59, 100)

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.MandelbrotOrbits")
  }
}