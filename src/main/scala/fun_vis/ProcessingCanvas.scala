package fun_vis

import fun_vis.Types._
import processing.core.PApplet


case class Origin(x: Int, y: Int)
case class Extent(width: Int, height: Int)
case class Canvas(width: Int, height: Int)

case class ProcessingCanvas(canvas: Canvas, extent: Extent, origin: Origin, applet: PApplet) {

  implicit def toPColor(c: Color): Int = c match {
    case RGBColor(r, g , b, a) => applet.color(r, g, b, a)
    case HSBColor(h, s, b, a) => applet.color(h, s, b, a)
  }

  val scaleX: Float = canvas.width * 1.0f / extent.width
  val scaleY: Float = canvas.width * 1.0f / extent.width

  def getPoint(x: Int, y: Int): Point = Point((x - origin.x) / scaleX, (y - origin.y) / scaleY)

  def foreach(im: Image[Color]): Unit = {
    for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
      val point = getPoint(x, y)
      val c = im(point)
      applet.set(x, y, c)
    }
  }
}

