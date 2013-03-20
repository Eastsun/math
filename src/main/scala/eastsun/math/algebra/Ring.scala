package eastsun.math.algebra

import scala.annotation.implicitNotFound

/**
 * A commutative ring with an identity element (one).
 */
@implicitNotFound(msg = "Cannot find Ring type class for ${T}")
trait Ring[T] extends Group[T] {
  
  def one: T
  def times(left: T, right: T): T
}

object Ring {
  
  object ByteIsRing extends Ring[Byte] {
    val zero = 0.toByte
    val one = 1.toByte
    def plus(left: Byte, right: Byte): Byte = (left + right).toByte
    def minus(left: Byte, right: Byte): Byte = (left - right).toByte
    def times(left: Byte, right: Byte): Byte = (left * right).toByte
    def negate(value: Byte): Byte = (-value).toByte
  }
  
  object CharIsRing extends Ring[Char] {
    val zero = 0.toChar
    val one = 1.toChar
    def plus(left: Char, right: Char): Char = (left + right).toChar
    def minus(left: Char, right: Char): Char = (left - right).toChar
    def times(left: Char, right: Char): Char = (left * right).toChar
    def negate(value: Char): Char = (-value).toChar
  }
  
  object ShortIsRing extends Ring[Short] {
    val zero = 0.toShort
    val one = 1.toShort
    def plus(left: Short, right: Short): Short = (left + right).toShort
    def minus(left: Short, right: Short): Short = (left - right).toShort
    def times(left: Short, right: Short): Short = (left * right).toShort
    def negate(value: Short): Short = (-value).toShort
  }
  
  object IntIsRing extends Ring[Int] {
    val zero = 0
    val one = 1
    def plus(left: Int, right: Int): Int = left + right
    def minus(left: Int, right: Int): Int = left - right
    def times(left: Int, right: Int): Int = left * right
    def negate(value: Int): Int = -value
  }
  
  object LongIsRing extends Ring[Long] {
    val zero = 0L
    val one = 1L
    def plus(left: Long, right: Long): Long = left + right
    def minus(left: Long, right: Long): Long = left - right
    def times(left: Long, right: Long): Long = left * right
    def negate(value: Long): Long = -value
  }
}