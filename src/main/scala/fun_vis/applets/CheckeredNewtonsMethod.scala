package fun_vis.applets

import fun_vis.ColorUtils.selectColor
import fun_vis.Types.ColorImage
import fun_vis._
import fun_vis.functions.Fractals
import fun_vis.functions.GrayFunctions.polarDist
import fun_vis.functions.MaskFunctions._
import fun_vis.utils.ComplexNumber
import processing.core.{PApplet, PConstants}


class CheckeredNewtonsMethod extends PApplet {

  implicit val colorMode: ColorMode = HSB

  override def setup(): Unit = {
    colorMode.set(this)
    background(360)
  }

  val pCanvas = ProcessingCanvas(
    Canvas(800, 600),
    Extent(-1, 1, -1, 1),
    this,
  )

  override def settings(): Unit = {
    size(pCanvas.canvas.width, pCanvas.canvas.height, PConstants.P2D)
  }

  override def draw(): Unit = {
    val f = (z: ComplexNumber) => z.pow(12) + (z.pow(4) * 10) - 12
    val fPrime = (z: ComplexNumber) => (z.pow(11) * 12) + (z.pow(3) * 40)
    val imageFn: ColorImage = p => {
      val iterations = Fractals.newtonsMethod(ComplexNumber(p.x, p.y), f, fPrime).iterations
      val rainbow = (polarDist(p) * 500) % 360
      selectColor(checker(p), HSBColor(rainbow, 0, 100 - iterations), HSBColor(rainbow, 0, iterations))

    }
    pCanvas.foreach(imageFn)
  }
}

object CheckeredNewtonsMethod extends PApplet {
  def main(args: Array[String]): Unit = {
    PApplet.main("fun_vis.applets.CheckeredNewtonsMethod")
  }
}
