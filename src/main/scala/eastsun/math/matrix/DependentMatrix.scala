package eastsun.math.matrix

import scala.reflect.ClassTag
import eastsun.math.algebra.Semigroup
import eastsun.math.algebra.Ring
import BooleanMatrix._

private[matrix] abstract class DependentMatrix[A: ClassTag](nRows: Int, nCols: Int)
  extends ArrayMatrix[A](nRows, nCols, null) {

  protected var isDependent: Boolean = true

  override def apply(ind: Int): A =
    if (isDependent) dependentApply(ind)
    else super.apply(ind)

  override def apply(ind: Index): A =
    if (isDependent) dependentApply(ind.atBound(numel))
    else super.apply(ind)

  override def apply(row: Int, col: Index): A =
    if (isDependent) dependentApply(row, col.atBound(cols))
    else super.apply(row, col)

  override def apply(row: Index, col: Int): A =
    if (isDependent) dependentApply(row.atBound(rows), col)
    else super.apply(row, col)

  override def apply(row: Index, col: Index): A =
    if (isDependent) dependentApply(row.atBound(rows), col.atBound(cols))
    else super.apply(row, col)

  override def apply(row: Int, col: Int): A =
    if (isDependent) dependentApply(row, col)
    else super.apply(row, col)

  override def update(ind: Int, value: A): Unit = {
    if (isDependent) reified()
    super.update(ind, value)
  }

  override def update(row: Int, col: Int, value: A): Unit = {
    if (isDependent) reified()
    super.update(row, col, value)
  }

  override def update(ind: Index, value: A): Unit = {
    if (isDependent) reified()
    super.update(ind, value)
  }

  override def update(row: Int, col: Index, value: A): Unit = {
    if (isDependent) reified()
    super.update(row, col, value)
  }

  override def update(row: Index, col: Int, value: A): Unit = {
    if (isDependent) reified()
    super.update(row, col, value)
  }

  override def update(row: Index, col: Index, value: A): Unit = {
    if (isDependent) reified()
    super.update(row, col, value)
  }

  override def update[Ind, V](ind: Ind, value: V)(implicit i: Indexed[Ind], v: Valued[A, V]): Unit = {
    if (isDependent) reified()
    super.update(ind, value)
  }

  override def update[Row, Col, V](row: Row, col: Col, value: V)(implicit r: Indexed[Row], c: Indexed[Col], v: Valued[A, V]): Unit = {
    if (isDependent) reified()
    super.update(row, col, value)
  }

  override def +(that: A)(implicit sg: Semigroup[A]): Matrix[A] = {
    if (isDependent) {
      new DirectMatrix(rows, cols, Array.tabulate(numel)(ind => sg.plus(dependentApply(ind), that)))
    } else super.+(that)
  }
  override def *(that: A)(implicit r: Ring[A]): Matrix[A] = {
    if (isDependent) {
      new DirectMatrix(rows, cols, Array.tabulate(numel)(ind => r.times(dependentApply(ind), that)))
    } else super.*(that)
  }

  override def map[B](f: A => B)(implicit ct: ClassTag[B]): Matrix[B] =
    if (isDependent) ct.toString match {
      case "Boolean" =>
        val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
        val g = f.asInstanceOf[A => Boolean]
        var ind = numel - 1
        while (ind >= 0) {
          if (g(dependentApply(ind))) buf(ind >> LogWL) |= 1L << ind
          ind -= 1
        }
        (new BooleanMatrix(rows, cols, buf)).asInstanceOf[Matrix[B]]
      case _ => new DirectMatrix(rows, cols, Array.tabulate(numel)(i => f(dependentApply(i))))
    }
    else super.map(f)

  override def >(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    if (isDependent) {
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      var ind = numel - 1
      while (ind >= 0) {
        if (ord.gt(dependentApply(ind), that)) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buf)
    } else super.>(that)
  }

  override def >=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    if (isDependent) {
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      var ind = numel - 1
      while (ind >= 0) {
        if (ord.gteq(dependentApply(ind), that)) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buf)
    } else super.>=(that)
  }
  
  override def <(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    if (isDependent) {
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      var ind = numel - 1
      while (ind >= 0) {
        if (ord.lt(dependentApply(ind), that)) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buf)
    } else super.>(that)
  }

  override def <=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    if (isDependent) {
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      var ind = numel - 1
      while (ind >= 0) {
        if (ord.lteq(dependentApply(ind), that)) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buf)
    } else super.>=(that)
  }


  override def unzip[T1, T2](implicit isPair: A =:= (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Matrix[T1], Matrix[T2]) = {
    if (isDependent) {
      val buf1 = Array.ofDim[T1](numel)
      val buf2 = Array.ofDim[T2](numel)
      var ind = numel - 1
      while (ind >= 0) {
        val (v1, v2) = isPair(dependentApply(ind))
        buf1(ind) = v1
        buf2(ind) = v2
        ind -= 1
      }
      (Matrix(rows, cols, buf1), Matrix(rows, cols, buf2))
    } else super.unzip
  }

  override def iterator: Iterator[A] =
    if (isDependent) Iterator.tabulate(numel)(i => dependentApply(i))
    else super.iterator

  /**
   * at least one of the following two methods should be override in sub class
   */
  protected def dependentApply(ind: Int): A = dependentApply(ind % rows, ind / rows)

  protected def dependentApply(row: Int, col: Int): A = dependentApply(row + col * rows)

  /**
   * should be override in sub class
   */
  protected def releaseDependent(): Unit

  override def reified(): Matrix[A] = {
    buffer = Array.ofDim[A](numel)
    var ind = numel - 1
    while (ind >= 0) {
      buffer(ind) = dependentApply(ind)
      ind -= 1
    }
    releaseDependent()
    isDependent = false
    this
  }
}

private[matrix] abstract class SingleDependentMatrix[A: ClassTag](nRows: Int, nCols: Int, protected var underingly: LinkableRectData[A])
  extends DependentMatrix[A](nRows, nCols) {

  underingly.linkBy(this)

  override protected def releaseDependent(): Unit = {
    underingly.unlink(this)
    underingly = null
  }
}

private[matrix] abstract class DoubleDependentMatrix[A: ClassTag](nRows: Int, nCols: Int, protected var underingly1: LinkableRectData[A], protected var underingly2: LinkableRectData[A])
  extends DependentMatrix[A](nRows, nCols) {

  underingly1.linkBy(this)
  underingly2.linkBy(this)

  override protected def releaseDependent(): Unit = {
    underingly1.unlink(this)
    underingly2.unlink(this)
    underingly1 = null
    underingly2 = null
  }
}


