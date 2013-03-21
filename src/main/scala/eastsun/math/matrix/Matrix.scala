package eastsun.math.matrix

import eastsun.math.algebra._
import scala.reflect.ClassTag
import BooleanMatrix._

trait Matrix[A] extends RectBuffer[A] {

  def rows: Int
  def cols: Int
  def length: Int
  def numel: Int
  def size: (Int, Int)

  def apply(ind: Int): A
  def apply(ind: Index): A
  def apply(row: Int, col: Int): A
  def apply(row: Int, col: Index): A
  def apply(row: Index, col: Int): A
  def apply(row: Index, col: Index): A
  def apply[Ind: Indexed](ind: Ind): Matrix[A]
  def apply[Row: Indexed, Col: Indexed](row: Row, col: Col): Matrix[A]

  def update(ind: Int, value: A): Unit
  def update(ind: Index, value: A): Unit
  def update(row: Int, col: Int, value: A): Unit
  def update(row: Int, col: Index, value: A): Unit
  def update(row: Index, col: Int, value: A): Unit
  def update(row: Index, col: Index, value: A): Unit
  def update[Ind, V](ind: Ind, value: V)(implicit i: Indexed[Ind], v: Valued[A, V]): Unit
  def update[Row, Col, V](row: Row, col: Col, value: V)(implicit r: Indexed[Row], c: Indexed[Col], v: Valued[A, V]): Unit

  def map[B: ClassTag](f: A => B): Matrix[B]
  def unzip[T1, T2](implicit isPair: A =:= (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Matrix[T1], Matrix[T2])

  protected[matrix] def asLinkable: LinkableRectData[A]

  def transpose: Matrix[A]
  def reshape(nRows: Int, nCols: Int): Matrix[A]

  override def clone: Matrix[A] = ???

  def reified(): Matrix[A]

  def +(that: Matrix[A])(implicit sg: Semigroup[A]): Matrix[A]
  def +=(that: Matrix[A])(implicit sg: Semigroup[A]): Matrix[A]
  def *(that: Matrix[A])(implicit r: Ring[A]): Matrix[A]
  def *=(that: Matrix[A])(implicit r: Ring[A]): Matrix[A] 
  def +(that: A)(implicit sg: Semigroup[A]): Matrix[A]
  def *(that: A)(implicit r: Ring[A]): Matrix[A]
  def kron(that: Matrix[A])(implicit r: Ring[A]): Matrix[A]

  def >(that: A)(implicit ord: Ordering[A]): Matrix[Boolean]
  def >=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean]
  def <(that: A)(implicit ord: Ordering[A]): Matrix[Boolean]
  def <=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean]

  override def equals(obj: Any): Boolean = obj match {
    case m: Matrix[_] => rows == m.rows && cols == m.cols &&
      (0 until numel).forall(i => apply(i) == m(i))
    case _ => false
  }
}

object Matrix extends LowerPriority {

  def apply[A: ClassTag, C <% Seq[A]](nRows: Int, nCols: Int, coll: C): Matrix[A] = {
    if (coll.size < nRows * nCols)
      throw new IndexOutOfBoundsException()
    new DirectMatrix(nRows, nCols, coll.toArray)
  }
  
  private[matrix] def apply[A](nRows: Int, nCols: Int, buffer: Array[A])(implicit ct: ClassTag[A]): Matrix[A] = ct.toString match {
    case "Boolean" =>
      val buf = buffer.asInstanceOf[Array[Boolean]]
      val bits = Array.ofDim[Long]((buffer.length + WordL -1) >> LogWL)
      var ind = buffer.length - 1
      while(ind >= 0) {
        if(buf(ind)) bits(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      (new BooleanMatrix(nRows, nCols, bits)).asInstanceOf[Matrix[A]]
    case _ => new DirectMatrix(nRows, nCols, buffer)
  }

  def rand(nRows: Int, nCols: Int): Matrix[Double] = {
    val array = Array.fill(nRows * nCols)(math.random)
    new DirectMatrix[Double](nRows, nCols, array)
  }

  def fill[A: ClassTag](nRows: Int, nCols: Int)(elem: A): Matrix[A] = {
    val under = LinkableRectData.fill(nRows, nCols)(elem)
    new SingleDependentMatrix(nRows, nCols, under) {
      @inline override def dependentApply(ind: Int) = underingly(ind)
      @inline override def dependentApply(row: Int, col: Int) = underingly(row, col)
    }
  }

  def tabulate[A](nRows: Int, nCols: Int)(f: (Int, Int) => A)(implicit ct: ClassTag[A]): Matrix[A] = ct.toString match {
    case "Boolean" =>
      val buf = Array.ofDim[Long]((nRows * nCols + WordL - 1) >> LogWL)
      val g = f.asInstanceOf[(Int, Int) => Boolean]
      var ind = nRows * nCols - 1
      while (ind >= 0) {
        if (g(ind % nRows, ind / nRows)) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      (new BooleanMatrix(nRows, nCols, buf)).asInstanceOf[Matrix[A]]
    case _ => new DirectMatrix(nRows, nCols, Array.tabulate(nRows * nCols)(ind => f(ind % nRows, ind / nRows)))
  }

  implicit def promote[U, V](m: Matrix[U])(implicit ev: U => V, ct: ClassTag[V]): Matrix[V] = m.map(ev)
  implicit class NumberOps[A](val num: A) extends AnyVal {
    def +(that: Matrix[A])(implicit sg: Semigroup[A], ct: ClassTag[A]): Matrix[A] = {
      new DirectMatrix(that.rows, that.cols, Array.tabulate(that.numel)(ind => sg.plus(num, that(ind))))
    }
    def -(that: Matrix[A])(implicit g: Group[A], ct: ClassTag[A]): Matrix[A] = {
      new DirectMatrix(that.rows, that.cols, Array.tabulate(that.numel)(ind => g.minus(num, that(ind))))
    }
    def *(that: Matrix[A])(implicit r: Ring[A], ct: ClassTag[A]): Matrix[A] = {
      new DirectMatrix(that.rows, that.cols, Array.tabulate(that.numel)(ind => r.times(num, that(ind))))
    }
    def /(that: Matrix[A])(implicit f: Field[A], ct: ClassTag[A]): Matrix[A] = {
      new DirectMatrix(that.rows, that.cols, Array.tabulate(that.numel)(ind => f.div(num, that(ind))))
    }
  }
}

sealed trait LowerPriority {

  def rand[A](nRows: Int, nCols: Int)(implicit ct: ClassTag[A], r: Ring[A]): Matrix[A] = {
    val rnd = new scala.util.Random
    val numel = nRows * nCols
    if (ct.toString == "Boolean") {
      val bits = Array.ofDim[Long]((numel + 63) >> 6)
      var cnt = bits.length - 1
      while (cnt >= 0) {
        bits(cnt) = rnd.nextLong
        cnt -= 1
      }
      return (new BooleanMatrix(nRows, nCols, bits)).asInstanceOf[Matrix[A]]
    }

    val array = (ct.toString match {
      case "Byte" =>
        val bytes = Array.ofDim[Byte](numel); rnd.nextBytes(bytes); bytes
      case "Char" => Array.fill(numel)(rnd.nextPrintableChar)
      case "Short" => Array.fill(numel)(rnd.nextInt.toShort)
      case "Int" => Array.fill(numel)(rnd.nextInt)
      case "Long" => Array.fill(numel)(rnd.nextLong)
      case "Float" => Array.fill(numel)(rnd.nextFloat)
      case "Double" => Array.fill(numel)(rnd.nextDouble)
      case other => throw new UnsupportedOperationException("Unsupported type: " + other)
    }).asInstanceOf[Array[A]]
    new DirectMatrix[A](nRows, nCols, array)
  }
}