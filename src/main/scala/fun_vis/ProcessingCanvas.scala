package fun_vis

import fun_vis.Types._
import processing.core.{PApplet, PConstants}


sealed trait ColorMode {
  def set(applet: PApplet): Unit
}

case object RGB extends ColorMode {
  def set(applet: PApplet): Unit = applet.colorMode(PConstants.RGB, 100)
}

case object HSB extends ColorMode {
  def set(applet: PApplet): Unit = applet.colorMode(PConstants.HSB, 360, 100, 100, 100)
}

case class Extent(xStart: Float, xEnd: Float, yStart: Float, yEnd: Float)

case class Canvas(width: Int, height: Int)

case class ProcessingCanvas(canvas: Canvas, extent: Extent, applet: PApplet) {

  implicit def toPColor(c: Color): Int = c match {
    case RGBColor(r, g, b, a) => applet.color(r, g, b, a)
    case HSBColor(h, s, b, a) => applet.color(h, s, b, a)
  }

  def map(v: Float, start1: Float, stop1: Float, start2: Float, stop2: Float): Float =
    start2 + (stop2 - start2) * (v - start1) / (stop1 - start1)

  def getPoint(x: Int, y: Int): Point = {
    val xPos =  map(x, 0, canvas.width, extent.xStart, extent.xEnd)
    val yPos = map(y, 0, canvas.height, extent.yStart, extent.yEnd)
    Point(xPos, yPos)
  }

  def foreach(im: Image[Color]): Unit = {
    for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
      val point = getPoint(x, y)
      val c = im(point)
      applet.set(x, y, c)
    }
  }
}

