package fun_vis.applets

import java.lang.Math._

import fun_vis._
import fun_vis.utils.ComplexNumber
import processing.core.PApplet.degrees
import processing.core.{PApplet, PConstants}


class DomainColoring extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(1000, 600),
    CustomExtent(-3, 3, -2, 2),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {

    val sinFunction = (p: Point) => {
      ComplexNumber(sin(p.x) * Math.cosh(p.y), cos(p.x) * Math.sinh(p.y)) +
        ComplexNumber(p.x, p.y).pow(1.0 / (frameCount + 4))
    }

    val colorFn = sinFunction andThen DomainColoring.getColor
    pCanvas.foreach(colorFn)
  }
}


object DomainColoring extends PApplet {
  val function = (p: Point) => {
    val x = ComplexNumber(p.x, p.y)
    (x.pow(2) - 1) * (x - 2 - ComplexNumber(0, 1).pow(2)) / (x.pow(2) + 2 + ComplexNumber(0, 1) * 2)
  }
  // Formula: https://mathematica.stackexchange.com/questions/7275
  def getColor(c: ComplexNumber): HSBColor = {
    val hue = degrees(atan2(c.r, c.i).toFloat)
    val saturation = abs(sin(2 * PI * c.abs))
    val b = pow(abs(sin(2 * PI * c.i) * sin(2 * PI * c.r)), 0.25)
    val brightness = 0.5 * ((1 - saturation) + b + Math.sqrt(Math.pow(1 - saturation - b, 2) + 0.01)) * 100
    HSBColor(hue.toFloat + 180, saturation.toFloat * 60, brightness.toFloat)
  }

  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.DomainColoring")
  }
}
