package eastsun.math.algebra

import scala.annotation.implicitNotFound

/**
 *  A monoid is an algebraic structure with addition operation and zero element.
 */
@implicitNotFound(msg = "Cannot find Monoid type class for ${T}")
trait Monoid[T] extends Semigroup[T] {
  def zero: T
  def plus(left: T, right: T): T
}