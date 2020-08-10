package fun_vis.functions

import fun_vis.ColorUtils.{interpolateColor, overlayColors, valueToHSB, valueToRGB, selectColor}
import fun_vis.{Color, ColorMode}
import fun_vis.Utils._
import fun_vis.functions.MaskFunctions._
import fun_vis.Mask._
import fun_vis.functions.GrayFunctions._
import fun_vis.Types.{ColorImage, GrayImage, MaskImage}


object ColorFunctions {
  def interpolateColorFn(g: GrayImage)(c1: ColorImage)(c2: ColorImage): ColorImage = lift3(interpolateColor)(g)(c1)(c2)

  def overlayColorFn(c1: ColorImage)(c2: ColorImage): ColorImage = lift2(overlayColors)(c1)(c2)

  def selectColorFn(m: MaskImage)(c1: ColorImage)(c2: ColorImage): ColorImage = lift3(selectColor)(m)(c1)(c2)

  def maskBlackWhite(m: MaskImage)(implicit c: ColorMode): ColorImage = selectColorFn(m)(lift0(Color.black))(lift0(Color.white))

  def grayToRGB(g: GrayImage): ColorImage = valueToRGB _ compose g

  def grayToHSB(g: GrayImage): ColorImage = valueToHSB _ compose g

  val maskAsColors: MaskImage => Color => Color => ColorImage =
    mask => ifTrue => ifFalse => selectColorFn(mask)(lift0(ifTrue))(lift0(ifFalse))

  def ybRings(implicit c: ColorMode): ColorImage = interpolateColorFn(wavDist)(lift0(Color.blue))(lift0(Color.yellow))

  def colorChecker(implicit c: ColorMode): ColorImage = maskAsColors(checker)(Color.black)(Color.white)

  def lerped(implicit c: ColorMode): ColorImage =
    interpolateColorFn(wavDist)(
      maskAsColors(checker)(Color.black)(Color.white))(
      maskAsColors(polarChecker)(Color.blue)(Color.yellow))

  def colorVStrip(implicit c: ColorMode): ColorImage = maskBlackWhite(vstrip)

  def colorCross(implicit c: ColorMode): ColorImage =
    overlayColorFn(
      maskAsColors(vstrip)(Color.black)(Color.invisible))(
      maskAsColors(hstrip)(Color.red)(Color.invisible))

  def checkerRingStrip(implicit c: ColorMode): ColorImage =
    selectColorFn(vstrip)(ybRings)(colorChecker)

  def inverseCheckerRingStrip(implicit c: ColorMode): ColorImage =
    selectColorFn(notMask(vstrip))(ybRings)(colorChecker)

  val grayColorGradient: Float => ColorImage =
    width => grayToRGB(grayGradient(width))

  def bigXChecker(implicit c: ColorMode): ColorImage =
    selectColorFn(bigX)(colorChecker)(lift0(Color.green))

  def ringsAsteriskOnChecker(implicit c: ColorMode): ColorImage =
    selectColorFn(asterisk)(ybRings)(colorChecker)
}