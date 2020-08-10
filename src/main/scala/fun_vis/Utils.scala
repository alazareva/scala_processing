package fun_vis

import fun_vis.Types.Image

object Utils {
  def lift0[A](v: A): Image[A] = _ => v

  def lift1[A, B](f: A => B)(img: Image[A]): Image[B] = f compose img

  def lift2[A, B, C](f: (A, B) => C)(imgA: Image[A])(imgB: Image[B]): Image[C] = p => f(imgA(p), imgB(p))

  def lift3[A, B, C, D](f: (A, B, C) => D)(imgA: Image[A])(imgB: Image[B])(imgC: Image[C]): Image[D] = p =>
    f(imgA(p), imgB(p), imgC(p))

}
