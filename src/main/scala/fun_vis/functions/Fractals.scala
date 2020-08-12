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


  def mandelbrot(maxIterations: Int = 1000, escape: Int = 2)(
    initialZ: ComplexNumber, f: (ComplexNumber, ComplexNumber) => ComplexNumber): Int =  {

    @tailrec
    def rec(z1: ComplexNumber, iteration: Int): Int = {
      if (z1.abs >= escape || iteration >= maxIterations) iteration
      else rec(f(z1, initialZ), iteration + 1)
    }
    rec(ComplexNumber(0, 0), 0)
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
