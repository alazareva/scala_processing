package fun_vis

import fun_vis.Types.Value


sealed trait Color

// TODO there are existing libs for this
object Color {
  def apply(v1: Value, v2: Value, v3: Value, v4: Value = 1)(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(v1, v2, v3, v4)
    case HSB => HSBColor(v1, v2, v3, v4)
  }

  def white(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(255, 255, 255)
    case HSB => HSBColor(0, 0, 100)
  }

  def black(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(0, 0, 0)
    case HSB => HSBColor(0, 0, 0)
  }

  def invisible(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(0, 0, 0, 0)
    case HSB => HSBColor(0, 0, 0, 0)
  }

  def red(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(255, 0, 0)
    case HSB => HSBColor(0, 100, 100)
  }

  def green(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(0, 255, 0)
    case HSB => HSBColor(120, 100, 100)
  }

  def blue(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(0, 0, 255)
    case HSB => HSBColor(240, 100, 100)
  }

  def yellow(implicit colorMode: ColorMode): Color = colorMode match {
    case RGB => RGBColor(255, 255, 0)
    case HSB => HSBColor(60, 100, 100)
  }
}

case class RGBColor(r: Value, g: Value, b: Value, a: Value = 100) extends Color
case class HSBColor(h: Value, s: Value, b: Value, a: Value = 100) extends Color

object ColorUtils {
  def interpolateColor(w: Value, c1: Color, c2: Color): Color = {
    def h(v1: Value, v2: Value): Value = (w * v1) + ((1 - w) * v2)

    (c1, c2) match {
      case (RGBColor(r1, g1, b1, a1), RGBColor(r2, g2, b2, a2)) => RGBColor(h(r1, r2), h(g1, g2), h(b1, b2), h(a1, a2))
      case (HSBColor(h1, s1, b1, a1), RGBColor(h2, s2, b2, a2)) => RGBColor(h(h1, h2), h(s1, s2), h(b1, b2), h(a1, a2))
    }
  }

  def overlayColors(c1: Color, c2: Color): Color = {
    def h(v1: Value, v2: Value, alpha: Value): Value = v1 + ((1 - alpha) * v2)

    (c1, c2) match {
      case (RGBColor(r1, g1, b1, a1), RGBColor(r2, g2, b2, a2)) => RGBColor(h(r1, r2, a1), h(g1, g2, a1), h(b1, b2, a1), h(a1, a2, a1))
      case (HSBColor(h1, s1, b1, a1), RGBColor(h2, s2, b2, a2)) => RGBColor(h(h1, h2, a1), h(s1, s2, a1), h(b1, b2, a1), h(a1, a2, a1))
    }
  }

  def selectColor(condition: Boolean, c1: Color, c2: Color): Color = if (condition) c1 else c2

  def valueToRGB(v: Value): RGBColor = RGBColor(v % 255, v % 255, v % 255)

  def valueToHSB(v: Value): HSBColor = HSBColor(v % 360, 50, 100, 100)
}