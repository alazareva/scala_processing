package fun_vis.applets

import fun_vis.Types.{ColorImage, Value}
import fun_vis.functions.ColorFunctions.grayToHSB
import fun_vis.functions.GrayFunctions.{polarDist, angle}
import fun_vis.{Canvas, ColorMode, Extent, HSB, HSBColor, ProcessingCanvas}
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}
import processing.core.PApplet.map

import scala.annotation.tailrec


class NewtonsMethod extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(800, 600),
    Extent(-2.5f, 2.5f, -2.5f, 2.5f),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val f = (z: ComplexNumber) => z.pow(7) + (z.pow(4) * 10) - 12
    val fPrime = (z: ComplexNumber) => (z.pow(6) * 7) + (z.pow(3) * 40)
    val imageFn: ColorImage = p => {
      val iterations = NewtonsMethod.newtonsMethod(ComplexNumber(p.x, p.y), f, fPrime)
      val hue_start = (50 + frameCount * 20) % 360
     // val h = map(iterations.iterations, 0, 100, 0, 1)
     val h = map(iterations.iterations, 0, 100, 0.9f, 1.1f)
      val rainbow = (polarDist(p) * 100 * h) % 360
      HSBColor(rainbow, 100, iterations.iterations)
    }
    pCanvas.foreach(imageFn)
  }
}

object NewtonsMethod extends PApplet {
  case class IterationResult(iterations: Int, c: ComplexNumber)

  def newtonsMethod(c: ComplexNumber,
                    f: ComplexNumber => ComplexNumber,
                    fPrime: ComplexNumber => ComplexNumber,
                    maxIterations: Int = 100,
                    tolerance: Float = 0.0000001f
                   ): IterationResult = {

    @tailrec
    def rec(z: ComplexNumber, iterations: Int): IterationResult = {
      if (iterations >= maxIterations) IterationResult(iterations, z)
      else {
        val step: ComplexNumber = f(z) / fPrime(z)
        if (step.abs < tolerance) IterationResult(iterations, z)
        else rec(z - step, iterations + 1)
      }
    }
    rec(c, 0)
  }
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.NewtonsMethod")
  }
}
