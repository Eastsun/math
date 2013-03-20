package eastsun.math.algebra

import scala.annotation.{implicitNotFound, tailrec}

/**
 * A semigroup is an algebraic structure consisting of a set together with addition operation.
 */
@implicitNotFound(msg = "Cannot find Semigroup type class for ${T}")
trait Semigroup[T] {
  def plus(left: T, right: T): T	
}

object Semigroup {
  
  implicit val byteIsRing = Ring.ByteIsRing
  implicit val charIsRing = Ring.CharIsRing
  implicit val shortIsRing = Ring.ShortIsRing 
  implicit val intIsRing = Ring.IntIsRing
  implicit val longIsRing = Ring.LongIsRing
  
  implicit val floatIsField = Field.FloatIsField
  implicit val doubleIsField = Field.DoubleIsField
  implicit val booleanIsField = Field.BooleanIsField
}