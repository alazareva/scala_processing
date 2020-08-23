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

    mandelbrotIterationResult(maxIterations, escape)(initialZ, f).iterations
  }

  def mandelbrotIterationResult(maxIterations: Int = 1000, escape: Int = 2)(
    initialZ: ComplexNumber, f: (ComplexNumber, ComplexNumber) => ComplexNumber): IterationResult =  {

    @tailrec
    def rec(z1: ComplexNumber, iteration: Int): IterationResult = {
      if (z1.abs >= escape || iteration >= maxIterations) IterationResult(iteration, z1)
      else rec(f(z1, initialZ), iteration + 1)
    }
    rec(ComplexNumber(0, 0), 0)
  }

  def mandelbrotOrbitTrap(maxIterations: Int = 1000, trapCenter: ComplexNumber)(
    initialZ: ComplexNumber, f: (ComplexNumber, ComplexNumber) => ComplexNumber): Float = {
    @tailrec
    def rec(z1: ComplexNumber, iteration: Int, distSoFar: Float): Float = {
      if (iteration >= maxIterations) distSoFar
      else rec(f(z1, initialZ), iteration + 1, Math.min(distSoFar, z1.dist(trapCenter).toFloat))
    }
    rec(ComplexNumber(0, 0), 0, Float.MaxValue)
  }

  def mandelbrotOrbitSize(maxIterations: Int = 1000, escape: Int = 2)(
    initialZ: ComplexNumber, f: (ComplexNumber, ComplexNumber) => ComplexNumber): (Boolean, Float) = {
    @tailrec
    def rec(z1: ComplexNumber, iteration: Int, distSoFar: Float): (Boolean, Float) = {
      if (iteration >= maxIterations || z1.abs >= escape) (z1.abs >= escape, distSoFar)
      else rec(f(z1, initialZ), iteration + 1, Math.max(distSoFar, z1.dist(initialZ).toFloat))
    }
    rec(ComplexNumber(0, 0), 0, 0)
  }

  // ported from: https://github.com/trevlovett/mauldin-gasket/blob/master/mauldin.c
  def mauldinGasket(xSize: Int, ySize: Int, maxIterations: Long = 2000000000L): List[Double] = {
    val image: Array[Double] = Array.fill(xSize *  ySize){0}
    val sqrt3 = 1.73205081f
    val sqrrt1p5 = 1.22474487f
    val x = Array(0.0, 0.0, 0.0)
    val y = Array(0.0, 0.0, 0.0)
    val c = 0.866025404f
    val scale = xSize * 0.5

    x(0) = Math.random()
    y(0) = Math.random()


    def plot(x: Double, y: Double, luma: Double): Unit = {
      val ix = (x * scale + xSize * 0.5).toInt
      val iy = (y * scale + xSize * 0.5).toInt
      if (ix >= 0 && iy >= 0 && ix < xSize && iy < ySize && luma > 0) {
        image(xSize * iy + ix) += luma
      }
    }

    def iterate(): Unit = {

      val choice = Math.abs(util.Random.nextInt * 0.5f).toInt % 3

      x(1) = -.5f * x(0) - c * y(0)
      y(1) = -.5f * y(0) + c * x(0)

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
      y(0) = (xa * yc + xc * ya) * sqrrt1p5

    }

    (0 to 100).foreach(_ => iterate())

    var i = 0L

    while (i < maxIterations){
      if (i % 1000000000 == 0) println(i)
      iterate()
      val fx = x(0) - x(0).toInt
      val fy = y(0) - y(0).toInt
      val btl = (1.0f - fx) * (1.0f - fy)
      val btr = fx * (1.0f - fy)
      val bbl = (1.0f - fx) * fy
      val bbr = fx * fy

      plot(x(0), y(0), btl)
      plot(x(0) + 1, y(0), btr)
      plot(x(0), y(0) + 1, bbl)
      plot(x(0) + 1, y(0) + 1, bbr)
      i += 1
    }
    image.toList
  }


  def julia(initialZ: ComplexNumber,
            f: ComplexNumber => ComplexNumber,
            maxIterations: Int=1000,
            maxZ: Int=10,
           ): Int =  {

    juliaIterationResult(initialZ, f, maxIterations, maxZ).iterations
  }


  def juliaIterationResult(initialZ: ComplexNumber,
            f: ComplexNumber => ComplexNumber,
            maxIterations: Int=1000,
            maxZ: Int=10,
           ): IterationResult =  {

    @tailrec
    def rec(z: ComplexNumber, iteration: Int): IterationResult = {
      if (z.abs >= maxZ || iteration > maxIterations) IterationResult(iteration, z)
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
