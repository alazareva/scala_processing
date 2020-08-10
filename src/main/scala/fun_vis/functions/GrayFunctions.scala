package fun_vis.functions

import fun_vis.PointUtils.{distFromOrigin, pointToPolar}
import fun_vis.Types.GrayImage
import processing.core.PConstants
import math.{abs, cos}

// example grayscale image definitions
object GrayFunctions {
  val wavDist: GrayImage = pt => ((1 + cos(PConstants.PI * distFromOrigin(pt))) / 2.0).toFloat

  val angle: GrayImage = pt => pointToPolar(pt).theta.toFloat

  val polarDist: GrayImage = pt => pointToPolar(pt).rho.toFloat

  val grayGradient: Float => GrayImage = width => pt => (abs(pt.x) / width).toFloat

}
