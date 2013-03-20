package eastsun.math.algebra

import scala.annotation.implicitNotFound

/**
 * A field is a commutative ring which contains a multiplicative inverse for every nonzero element
 */
@implicitNotFound(msg = "Cannot find Field type class for ${T}")
trait Field[T] extends Ring[T]{

  def inverse(value: T): T
  
  def div(left: T, right: T): T
}

object Field {
  
  object FloatIsField extends Field[Float] {
    val zero = 0f
    val one = 1f    
    def plus(left: Float, right: Float): Float = left + right   
    def negate(value: Float): Float = -value   
    def minus(left: Float, right: Float): Float = left - right   
    def times(left: Float, right: Float): Float = left * right   
    def div(left: Float, right: Float): Float = left / right  
    def inverse(value: Float): Float = 1/value
  }
  
  object DoubleIsField extends Field[Double] {
    val zero = 0.0
    val one = 1.0
    def plus(left: Double, right: Double): Double = left + right
    def negate(value: Double): Double = -value
    def minus(left: Double, right: Double): Double = left - right
    def times(left: Double, right: Double): Double = left * right
    def div(left: Double, right: Double): Double = left / right
    def inverse(value: Double): Double = 1/value
  }
  
  object BooleanIsField extends Field[Boolean] {
    val zero = false
    val one = true
    def plus(left: Boolean, right: Boolean): Boolean = left ^ right
    def negate(value: Boolean): Boolean = value
    def minus(left: Boolean, right: Boolean): Boolean = left ^ right
    def times(left: Boolean, right: Boolean): Boolean = left & right
    
    def div(left: Boolean, right: Boolean): Boolean = 
      if(right)  left
      else throw new ArithmeticException("Divided by zero.")
    
    def inverse(value: Boolean): Boolean =
      if(value)  value
      else throw new ArithmeticException("Divided by zero.")
  }
}