package fun_vis.utils


case class ComplexNumber(r: Double, i: Double) {
  override def toString: String = f"($r + $i i)"

  def abs: Double = Math.hypot(r, i)

  def +(other: ComplexNumber) = ComplexNumber(r + other.r, i + other.i)

  def +(other: Double): ComplexNumber = this + ComplexNumber(other, 0)

  def -(other: ComplexNumber) = ComplexNumber(r - other.r, i - other.i)

  def -(other: Double): ComplexNumber = this - ComplexNumber(other, 0)

  def *(other: ComplexNumber): ComplexNumber = {
    val new_r = r * other.r - i * other.i
    val new_i = r * other.i + i * other.r
    ComplexNumber(new_r, new_i)
  }

  def *(other: Double): ComplexNumber = this * ComplexNumber(other, 0)

  def pow(p: Double): ComplexNumber = {
    val log_r = Math.log(Math.sqrt(r * r + i * i))
    val log_i = Math.atan2(i, r)
    val p_log_r = p * log_r
    val p_log_i = p * log_i
    val e_p_log_r = Math.exp(p_log_r)
    ComplexNumber(e_p_log_r * Math.cos(p_log_i), e_p_log_r * Math.sin(p_log_i))
  }

  def /(other: ComplexNumber): ComplexNumber = {
    val denom = other.r * other.r + other.i * other.i
    this * ComplexNumber(other.r / denom, -other.i / denom)
  }

  def /(other: Double): ComplexNumber = this / ComplexNumber(other, 0)

  def round(dec: Int): ComplexNumber = {
    val mult = Math.pow(10, dec)
    ComplexNumber(r * mult.round / mult, i * mult.round / mult)
  }
}

import fun_vis.applets.NewtonsMethod.newtonsMethod

object Main extends App {
  val z = ComplexNumber(0, 1)
  val f = (z: ComplexNumber) => z.pow(3) - 1
  val fPrime = (z: ComplexNumber) => z.pow(2) * 3
  println(newtonsMethod(z, f, fPrime))
}