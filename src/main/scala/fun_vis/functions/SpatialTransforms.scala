package fun_vis.functions

import fun_vis.PointUtils.{rotatePoint, scalePoint, swirlPoint, translatePoint}
import fun_vis.Types.{Image, Value}
import fun_vis.{Color, Point, Polar, Vector}

object SpatialTransforms {
  type TransformPoint = Point => Point
  type TransformPolar = Polar => Polar
  type MapImage[A] = Image[A] => Image[A]
  type MapMask = MapImage[Boolean]
  type MapGray = MapImage[Value]
  type MapColor = MapImage[Color]


  def translate[A](v: Vector): MapImage[A] = im => p => im(translatePoint(-v)(p))

  def scale[A](v: Vector): MapImage[A] = im => p => im(scalePoint(v.inverse)(p))

  def rotate[A](d: Double): MapImage[A] = im => p => im(rotatePoint(-d)(p))

  def swirl[A](d: Double): MapImage[A] = im => p => im(swirlPoint(-d)(p))

}