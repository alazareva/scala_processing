package fun_vis.applets

import fun_vis.ColorUtils.selectColor
import fun_vis.Mask.xorMask
import fun_vis.PointUtils.scalePoint
import fun_vis.Types.{ColorImage, Value}
import fun_vis.{Canvas, Color, ColorMode, ColorUtils, Extent, HSB, HSBColor, Point, ProcessingCanvas, Vector}
import fun_vis.functions.ColorFunctions.grayToHSB
import fun_vis.functions.GrayFunctions.{angle, polarDist}
import fun_vis.functions.MaskFunctions.{altRingsOf, checkerOf}
import processing.core.{PApplet, PConstants}

import scala.annotation.tailrec


class RainbowMandelbrot extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(800, 500),
    Extent(-3, 2, -1.5f, 1.5f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 1000
    val colorFunction: ColorImage = p => {
      val iterations = RainbowMandelbrot.mandelbrot(scalePoint(Vector(0.7f, 0.7f))(p), maxIterations)
      val scalePolarDistance = (v: Value) => v * 300.0f + frameCount * 20
      val rainbow = grayToHSB(polarDist andThen scalePolarDistance)(p)
      val mask = xorMask(checkerOf(angle(p)))(altRingsOf(frameCount % 50 + 2))
      ColorUtils.selectColor(
        mask(p),
        selectColor(iterations == maxIterations, Color.black, RainbowMandelbrot.pink),
        selectColor(iterations == maxIterations, Color.white, rainbow)
      )
    }
    pCanvas.foreach(colorFunction)
  }
}

object RainbowMandelbrot extends PApplet {

  val pink = HSBColor(330, 59, 100)

  def mandelbrot(p: Point, maxIteration: Int = 1000, escape: Int = 2): Int = {
    @tailrec
    def rec(pt: Point, ip: Point, count: Int): Int = {
      if (((pt.x * pt.x + pt.y * pt.y) >= escape) || count >= maxIteration) count
      else rec(Point(pt.x * pt.x - pt.y * pt.y + ip.x, 2 * pt.x * pt.y + ip.y), ip, count + 1)
    }

    rec(Point(0, 0), p, 0)
  }
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.RainbowMandelbrot")
  }
}