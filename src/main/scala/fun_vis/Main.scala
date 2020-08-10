package fun_vis

import fun_vis.Mask._
import fun_vis.functions.ColorFunctions._
import fun_vis.functions.GrayFunctions._
import fun_vis.functions.MaskFunctions._
import fun_vis.PointUtils._
import fun_vis.Utils._
import fun_vis.Types._
import processing.core.{PApplet, PConstants}

import scala.annotation.tailrec



sealed trait ColorMode {
  def set(applet: PApplet): Unit
}

case object RGB extends ColorMode {
  def set(applet: PApplet): Unit = applet.colorMode(PConstants.RGB, 100)
}

case object HSB extends ColorMode {
  def set(applet: PApplet): Unit = applet.colorMode(PConstants.HSB, 360, 100, 100, 100)
}


object SketchFunctions {

  val pink = HSBColor(330, 59, 100)

  def mandelbrot(p: Point, maxIteration: Int = 1000, escape: Int = 2): Int = {
    @tailrec
    def rec(pt: Point, ip: Point, count: Int): Int={
      if (((pt.x*pt.x+pt.y*pt.y) >= escape) || count >= maxIteration) count
      else rec(Point(pt.x*pt.x-pt.y*pt.y+ip.x, 2*pt.x*pt.y+ip.y),ip, count+1)
    }
    rec(Point(0,0) , p, 0)
  }
}


class Display extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(800, 500),
    Extent(5, 3),
    Origin(400, 250),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val maxIterations = 1000
    val colorFunction: ColorImage = p => {
      val iterations = SketchFunctions.mandelbrot(scalePoint(Vector(0.7f, 0.7f))(p), maxIterations)
      val scaleValue = (v: Value) =>  v * 300.0f + frameCount * 20
      val hue = grayToHSB(polarDist andThen scaleValue)
      val selectColor1 = lift0(ColorUtils.selectColor(iterations == maxIterations, Color.black, SketchFunctions.pink))
      val selectColor2 = lift0(ColorUtils.selectColor(iterations == maxIterations, Color.white, hue(p)))
      val fn = selectColorFn(xorMask(checkerOf(angle(p)))(altRingsOf(frameCount % 50 + 2)))(selectColor1)(selectColor2)
      fn(p)
    }
    pCanvas.foreach(colorFunction)
  }

}

object Display extends PApplet {
  def main(args:Array[String]): Unit = {
    PApplet.main("fun_vis.Display")
  }
}