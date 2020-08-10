package fun_vis

import fun_vis.Utils.{lift0, lift1, lift2}
import fun_vis.Types.{Image, MaskImage}

object Mask {

  def fullMask: Image[Boolean] = lift0(true)

  def emptyMask: Image[Boolean] = lift0(false)

  def notMask(m: MaskImage): MaskImage = lift1((b: Boolean) => !b)(m)

  def intersectMask(m1: MaskImage)(m2: MaskImage): MaskImage = lift2((b1: Boolean, b2: Boolean) => b1 && b2)(m1)(m2)

  def unionMask(m1: MaskImage)(m2: MaskImage): MaskImage = lift2((b1: Boolean, b2: Boolean) => b1 || b2)(m1)(m2)

  def xorMask(m1: MaskImage)(m2: MaskImage): MaskImage = lift2((b1: Boolean, b2: Boolean) => b1 ^ b2)(m1)(m2)

  def diffMask(m1: MaskImage)(m2: MaskImage): MaskImage = intersectMask(m1)(notMask(intersectMask(m1)(m2)))
}
