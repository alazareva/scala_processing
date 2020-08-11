package fun_vis.functions

import fun_vis.Point
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

  def mandelbrot(maxIterations: Int = 1000, escape: Int = 2)(p: Point): Int = {
    @tailrec
    def rec(pt: Point, ip: Point, count: Int): Int = {
      if (((pt.x * pt.x + pt.y * pt.y) >= escape) || count >= maxIterations) count
      else rec(Point(pt.x * pt.x - pt.y * pt.y + ip.x, 2 * pt.x * pt.y + ip.y), ip, count + 1)
    }

    rec(Point(0, 0), p, 0)
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
