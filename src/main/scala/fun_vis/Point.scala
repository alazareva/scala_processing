package fun_vis

import fun_vis.functions.SpatialTransforms.{TransformPoint, TransformPolar}
import fun_vis.utils.ComplexNumber

import math.{Pi, atan2, cos, pow, sin, sqrt}

// a point in the cartesian coordinate system
case class Point(x: Double, y: Double)

// a point in the polar coordinate system
case class Polar(rho: Double, theta: Double)


case class Vector(x: Float, y: Float) {
  def unary_- = Vector(-x, -y)

  def inverse = Vector(1 / x, 1 / y)
}


object PointUtils {

  def distFromOrigin(pt: Point): Float = sqrt(
    pow(pt.x, 2) + pow(pt.y, 2)
  ).toFloat

  def pointToPolar(pt: Point): Polar = Polar(distFromOrigin(pt), atan2(pt.y, pt.x))

  def polarToPoint(plr: Polar): Point = Point(plr.rho * cos(plr.theta), plr.rho * sin(plr.theta))

  def pointToComplexNumber(p: Point): ComplexNumber = ComplexNumber(p.x, p.y)

  def complexNumberToPoint(z: ComplexNumber): Point = Point(z.r, z.i)

  def translatePoint(v: Vector): TransformPoint = p => Point(p.x + v.x, p.y + v.y)

  def scalePoint(v: Vector): TransformPoint = p => Point(p.x * v.x, p.y * v.y)

  def rotatePoint(d: Double): TransformPoint = p => Point(p.x * cos(d) - p.y * sin(d), p.y * cos(d) + p.x * sin(d))

  def swirlPoint(d: Double): TransformPoint = p => rotatePoint(distFromOrigin(p) * (2 * Pi / d))(p)

  def polarTransformToPoint(t: TransformPolar): TransformPoint = polarToPoint _ compose t compose pointToPolar

}