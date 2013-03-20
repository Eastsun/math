package eastsun.math.algebra

import scala.annotation.implicitNotFound

/**
 * A group is a set together with addition operation that satisfy four conditions called the group axioms, 
 * namely closure, associativity, identity and invertibility. 
 */
@implicitNotFound(msg = "Cannot find Group type class for ${T}")
trait Group[T] extends Monoid[T] {
  def negate(value: T): T
  def minus(left: T, right: T): T
}