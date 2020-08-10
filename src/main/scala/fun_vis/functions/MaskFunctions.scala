package fun_vis.functions

import fun_vis.PointUtils.{distFromOrigin, polarTransformToPoint, scalePoint, translatePoint, pointToPolar}
import fun_vis.functions.SpatialTransforms.{MapMask, rotate, scale, swirl, translate}
import fun_vis.Mask._
import fun_vis.Types.{MaskImage, Value}
import fun_vis.{Point, Polar, Vector}
import processing.core.PConstants

// example mask image definitions
object MaskFunctions {

  val vstrip: MaskImage = pt => Math.abs(pt.x) <= 0.5

  val hstrip: MaskImage = pt => Math.abs(pt.y) <= 0.5

  val checker: MaskImage = pt => (Math.floor(pt.x) + Math.floor(pt.y)).toInt % 2 == 0

  def checkerOf(i: Value): MaskImage = pt => (Math.floor(pt.x * i) + Math.floor(pt.y * i)).toInt % 2 == 0

  val altRings: MaskImage =
    pt => Math.floor(distFromOrigin(pt)).toInt % 2 == 0

  def altRingsOf(i: Int): MaskImage = pt => Math.floor(distFromOrigin(pt) * 0.2 * i).toInt % 2 == 0

  val polarChecker: MaskImage = {
    val sc = (plr: Polar) => Point(plr.rho, plr.theta * (10 / PConstants.PI))
    checker compose sc compose pointToPolar
  }

  val udisk: MaskImage = pt => distFromOrigin(pt) < 1

  val scaledisk: MaskImage =
    udisk compose scalePoint(Vector(2, 2))

  val transdisk: MaskImage = udisk compose translatePoint(Vector(1, 0))

  val filtscaledisk: MaskImage = scale(Vector(2, 2))(udisk)

  val filttransdisk: MaskImage = translate(Vector(1, 0))(udisk)

  val swirled: MaskImage = swirl(1)(vstrip)

  def swirlBy(d: Double): MaskImage = swirl(d)(vstrip)

  val annulus: Value => MaskImage = sc => diffMask(udisk)(scale(Vector(sc, sc))(udisk))

  val radReg: Int => MaskImage =
    n => {
      val test = (plr: Polar) => Math.floor(plr.theta * (n / PConstants.PI)).toInt % 2 == 0
      test compose pointToPolar
    }

  val wedgeAnnulus: MaskImage = intersectMask(annulus(0.25f))(radReg(7))

  val shiftXor: Value => MapMask =
    dx => img => xorMask(
      translate(Vector(dx, 0))(img))(
      translate(Vector(-dx, 0))(img)
    )

  val shifted: MaskImage = shiftXor(2.6f)(altRings)

  def shiftedBy(i: Int): MaskImage = shiftXor(i * 0.1f)(altRings)

  val rotatedLine: Float => MaskImage = amount => rotate(amount)(vstrip)

  val bigX: MaskImage = unionMask(rotatedLine(0.45f))(rotatedLine(-0.45f))

  val asterisk: MaskImage = unionMask(unionMask(vstrip)(rotatedLine(-0.90f)))(rotatedLine(0.90f))

  val inverseAsterisk: MaskImage = xorMask(asterisk)(fullMask)

  val recordShape: MaskImage = {
    val fun = (plr: Polar) => Polar(math.abs(plr.rho - 1.5), plr.theta)
    udisk compose polarTransformToPoint(fun)
  }

  val asteriskMinusX: MaskImage = diffMask(diffMask(asterisk)(bigX))(emptyMask)
}