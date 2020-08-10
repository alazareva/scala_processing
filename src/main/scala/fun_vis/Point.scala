package fun_vis

import fun_vis.functions.SpatialTransforms.{TransformPoint, TransformPolar}

// a point in the cartesian coordinate system
case class Point(x: Double, y: Double)

// a point in the polar coordinate system
case class Polar(rho: Double, theta: Double)




case class Vector(x: Float, y: Float) {
  def unary_- = Vector(-x, -y)
  def inverse = Vector(1 / x, 1 / y)
}


object PointUtils {

  def distFromOrigin(pt: Point): Float = Math.sqrt(
    Math.pow(pt.x, 2) + Math.pow(pt.y, 2)
  ).toFloat

  def pointToPolar(pt: Point): Polar = Polar(distFromOrigin(pt), Math.atan2(pt.y, pt.x))

  def polarToPoint(plr: Polar): Point = Point(plr.rho * Math.cos(plr.theta), plr.rho * Math.sin(plr.theta))

  def translatePoint(v: Vector): TransformPoint = p => Point(p.x + v.x, p.y + v.y)

  def scalePoint(v: Vector): TransformPoint = p => Point(p.x * v.x, p.y * v.y)

  def rotatePoint(d: Double): TransformPoint = p => Point(p.x * Math.cos(d) - p.y * Math.sin(d), p.y * Math.cos(d) + p.x * Math.sin(d))

  def swirlPoint(d: Double): TransformPoint = p => rotatePoint(distFromOrigin(p) * (2 * Math.PI / d))(p)

  def polarTransformToPoint(t: TransformPolar): TransformPoint = polarToPoint _ compose t compose pointToPolar

}