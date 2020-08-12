package fun_vis.functions

import fun_vis.Point
import scala.util
import fun_vis.utils.ComplexNumber

import scala.annotation.tailrec

object Fractals {

  case class IterationResult(iterations: Int, c: ComplexNumber)

  def newtonsMethod(c: ComplexNumber,
                    f: ComplexNumber => ComplexNumber,
                    fPrime: ComplexNumber => ComplexNumber,
                    maxIterations: Int = 100,
                    tolerance: Float = 0.0000001f
                   ): IterationResult = {

    @tailrec
    def rec(z: ComplexNumber, iterations: Int): IterationResult = {
      if (iterations >= maxIterations) IterationResult(iterations, z)
      else {
        val step: ComplexNumber = f(z) / fPrime(z)
        if (step.abs < tolerance) IterationResult(iterations, z)
        else rec(z - step, iterations + 1)
      }
    }
    rec(c, 0)
  }


  def mandelbrot(maxIterations: Int = 1000, escape: Int = 2)(
    initialZ: ComplexNumber, f: (ComplexNumber, ComplexNumber) => ComplexNumber): Int =  {

    @tailrec
    def rec(z1: ComplexNumber, iteration: Int): Int = {
      if (z1.abs >= escape || iteration >= maxIterations) iteration
      else rec(f(z1, initialZ), iteration + 1)
    }
    rec(ComplexNumber(0, 0), 0)
  }

  // https://github.com/trevlovett/mauldin-gasket/blob/master/mauldin.c
  def maudlinGasket(image: Array[Double], xSize: Int, ySize: Int, maxIterations: Int = 200000000): Array[Double] = {
    val sqrt3 = 1.73205081f
    val sqrrt1p5 = 1.22474487f
    val x = Array(0.0, 0.0, 0.0)
    val y = Array(0.0, 0.0, 0.0)
    val c = 0.866025404f


    def plot(x: Double, y: Double, luma: Double): Unit = {
      val ix = x.toInt
      val iy = y.toInt
      val luma_pos = List(luma, 1.0).max.toInt
      if (ix >= 0 && iy >= 0 && ix < xSize && iy < ySize) {
        image(ySize * iy + ix) += luma_pos
      }
    }

    def iterate(): Unit = {
      x(0) = Math.random()
      y(0) = Math.random()

      val choice = Math.abs(util.Random.nextInt) % 3

      x(1) = -.5f * x(0) - c * y(0)
      y(1) = -.5f * y(0) + c * y(0)

      x(2) = x(1) * x(1) - y(1) * y(1)
      y(2) = 2.0f * x(1) * y(1)

      val xa = (sqrt3 - 1.0f) * x(choice) + 1.0f
      val ya = (sqrt3 - 1.0f) * y(choice)

      val xb = -x(choice) + (sqrt3 + 1.0f)
      val yb = -y(choice)

      val fact = 1.0f / (xb * xb + yb * yb)
      val xc = xb * fact
      val yc = -yb * fact
      x(0) = (xa * xc - ya * yc) * sqrrt1p5
      y(0) = (xa * yc - xc * ya) * sqrrt1p5

    }

    (0 to 100).foreach(_ => iterate())

    (100 to maxIterations).foreach{_ =>
      iterate()
      val fx = x(0) - x(0).toInt
      val fy = y(0) - y(0).toInt
      val btl = (1.0f - fx) * (1.0f - fy)
      val btr = fx * (1.0f - fy)
      val bbl = (1.0f - fx) * fy
      val bbr = fx * fy

      plot(x(0), y(0), btl)
      plot(x(0) + 1, y(0),   btr)
      plot(x(0), y(0) + 1, bbl)
      plot(x(0) + 1, y(0) + 1, bbr)
    }
    image
  }


  def julia(initialZ: ComplexNumber,
            f: ComplexNumber => ComplexNumber,
            maxIterations: Int=1000,
            maxZ: Int=10,
           ): Int =  {

    @tailrec
    def rec(z: ComplexNumber, iteration: Int): Int = {
      if (z.abs >= maxZ || iteration > maxIterations) iteration
      else rec(f(z), iteration + 1)
    }
    rec(initialZ, 0)
  }

  def phoenix(initialZ: ComplexNumber,
              f: (ComplexNumber, ComplexNumber) => ComplexNumber,
              maxIterations: Int=100,
              maxZ: Int=10,
             ): Int =  {

    def rec(z1: ComplexNumber, z2: ComplexNumber, iteration: Int): Int = {
      if (z1.abs >= maxZ || iteration > maxIterations) iteration
      else rec(f(z1, z2), z1, iteration + 1)
    }
    rec(initialZ, initialZ, 0)
  }
}
