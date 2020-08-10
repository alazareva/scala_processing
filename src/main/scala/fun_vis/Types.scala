package fun_vis

object Types {

  type Value = Float
  type Image[A] = Point => A
  type MaskImage = Image[Boolean]
  type GrayImage = Image[Value]
  type ColorImage = Image[Color]
}