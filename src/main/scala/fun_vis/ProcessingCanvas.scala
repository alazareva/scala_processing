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


sealed trait Extent

case class CustomExtent(xStart: Float, xEnd: Float, yStart: Float, yEnd: Float) extends Extent {
  def *(m: Float): CustomExtent = CustomExtent(xStart * m, xEnd * m, yStart * m, yEnd * m)
  def +(v: Vector): CustomExtent = CustomExtent(xStart + v.x, xEnd + v.x, yStart + v.y, yEnd + v.y)
}

object CustomExtent {
  def apply(w: Float, h: Float): CustomExtent = CustomExtent(-w, w, -h, h)
}

case object CanvasExtent extends Extent

case class Canvas(width: Int, height: Int) {
  def /(d: Float):Canvas = Canvas((width / d).toInt, (height / d).toInt)
  def *(d: Float):Canvas = Canvas((width * d).toInt, (height * d).toInt)
}

case class ProcessingCanvas(canvas: Canvas, extent: Extent, applet: PApplet) {

  implicit def toPColor(c: Color): Int = c match {
    case RGBColor(r, g, b, a) => applet.color(r, g, b, a)
    case HSBColor(h, s, b, a) => applet.color(h, s, b, a)
  }

  def map(v: Float, start1: Float, stop1: Float, start2: Float, stop2: Float): Float =
    start2 + (stop2 - start2) * (v - start1) / (stop1 - start1)

  def getPoint(x: Int, y: Int): Point = extent match {
    case CustomExtent(xStart, xEnd, yStart, yEnd) => {
      val xPos = map(x, 0, canvas.width, xStart, xEnd)
      val yPos = map(y, 0, canvas.height, yStart, yEnd)
      Point(xPos, yPos)
    }
    case CanvasExtent => Point(x, y)
  }

  def foreach(im: Image[Color]): Unit = {
    for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
      val point = getPoint(x, y)
      val c = im(point)
      applet.set(x, y, c)
    }
  }
}

