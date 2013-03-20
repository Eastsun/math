package eastsun.math.matrix

import eastsun.math.algebra.{ Semigroup, Ring }
import scala.reflect.ClassTag
import scala.collection.mutable
import scala.ref.WeakReference
import BooleanMatrix._

protected[matrix] abstract class ArrayMatrix[A: ClassTag](val rows: Int, val cols: Int, protected var buffer: Array[A])
  extends LinkableRectData[A] with Matrix[A] { self =>

  private[this] val links = mutable.Set.empty[WeakReference[Matrix[_]]]

  final override def linkBy(m: Matrix[_]): Unit = links += WeakReference(m)

  final override def unlink(m: Matrix[_]): Unit =
    links.retain {
      case WeakReference(n) => !(m eq n)
      case _ => false
    }

  final override def reifyLinks(): Unit = {
    if (links.nonEmpty) {
      for (WeakReference(m) <- links)
        m.reified()
      links.clear()
    }
  }

  def length: Int = math.max(rows, cols)

  def numel: Int = rows * cols

  def size: (Int, Int) = (rows, cols)

  def apply(ind: Int): A = buffer(ind)

  def apply(ind: Index): A = buffer(ind.atBound(numel))

  def apply(row: Int, col: Int): A = buffer(row + col * rows)

  def apply(row: Int, col: Index): A = buffer(row + col.atBound(cols) * rows)

  def apply(row: Index, col: Int): A = buffer(row.atBound(rows) + col * rows)

  def apply(row: Index, col: Index): A = buffer(row.atBound(rows) + col.atBound(cols) * rows)

  def apply[Ind](ind: Ind)(implicit i: Indexed[Ind]): Matrix[A] = {
    val ii = i.atBound(rows, cols, ind)
    new SingleDependentMatrix(ii.rows, ii.cols, this) {
      @inline override def dependentApply(row: Int, col: Int): A = underingly(ii(row, col))
      @inline override def dependentApply(ind: Int): A = underingly(ii(ind))
    }
  }

  def apply[Row, Col](row: Row, col: Col)(implicit r: Indexed[Row], c: Indexed[Col]): Matrix[A] = {
    val ri = r.atBound(rows, row)
    val ci = c.atBound(cols, col)
    new SingleDependentMatrix(ri.numel, ci.numel, this) {
      @inline override def dependentApply(row: Int, col: Int): A = underingly(ri(row), ci(col))
    }
  }

  def update(ind: Int, value: A): Unit = {
    reifyLinks()
    buffer(ind) = value
  }

  def update(row: Int, col: Int, value: A): Unit = {
    reifyLinks()
    buffer(row + col * rows) = value
  }

  def update(ind: Index, value: A): Unit = update(ind.atBound(numel), value)

  def update(row: Int, col: Index, value: A): Unit = update(row, col.atBound(cols), value)
  def update(row: Index, col: Int, value: A): Unit = update(row.atBound(rows), col, value)
  def update(row: Index, col: Index, value: A): Unit = update(row.atBound(rows), col.atBound(cols), value)

  def update[Ind, V](ind: Ind, value: V)(implicit i: Indexed[Ind], v: Valued[A, V]): Unit = {
    reifyLinks()
	val ii = i.atBound(rows, cols, ind)
	val vv = v.fitSize(ii.numel, value)
	for(idx <- 0 until ii.numel) {
	  buffer(ii(idx)) = vv(idx)
	}
  }

  def update[Row, Col, V](row: Row, col: Col, value: V)(implicit r: Indexed[Row], c: Indexed[Col], v: Valued[A, V]): Unit = {
	reifyLinks()
	val ri = r.atBound(rows, row)
	val ci = c.atBound(cols, col)
	val vv = v.fitSize(ri.numel, ci.numel, value)
	for(rIdx <- 0 until ri.numel; cIdx <- 0 until ci.numel) {
	  buffer(ri(rIdx) + ci(cIdx)*rows) = vv(rIdx, cIdx)
	}
  }

  def map[B](f: A => B)(implicit ct: ClassTag[B]): Matrix[B] = ct.toString match {
    case "Boolean" => 
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      val g = f.asInstanceOf[A => Boolean]
      var ind = numel - 1
      while(ind >= 0) {
        if(g(buffer(ind))) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      (new BooleanMatrix(rows, cols, buf)).asInstanceOf[Matrix[B]]
    case _ => new DirectMatrix(rows, cols, buffer.map(f))
  }
  
  def unzip[T1, T2](implicit isPair: A =:= (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Matrix[T1], Matrix[T2]) = {
    val buf1 = Array.ofDim[T1](numel)
    val buf2 = Array.ofDim[T2](numel)
    var ind = numel - 1
    while(ind >= 0) {
      val (v1, v2) = isPair(buffer(ind))
      buf1(ind) = v1
      buf2(ind) = v2
      ind -= 1
    }
    (Matrix(rows, cols, buf1), Matrix(rows, cols, buf2))
  }

  override def asLinkable: LinkableRectData[A] = this

  def iterator: Iterator[A] = buffer.iterator

  def transpose: Matrix[A] = new SingleDependentMatrix[A](cols, rows, this) {
    @inline override def dependentApply(row: Int, col: Int): A = underingly(col, row)
  }

  def reshape(nRows: Int, nCols: Int): Matrix[A] = new SingleDependentMatrix[A](nRows, nCols, this) {
    if (self.numel != numel)
      throw new MatrixSizeMatchError("To RESHAPE the number of elements must not change.")
    @inline override def dependentApply(ind: Int): A = underingly(ind)
  }

  def kron(that: Matrix[A])(implicit r: Ring[A]): Matrix[A] = new DoubleDependentMatrix[A](rows * that.rows, cols * that.cols, this, that.asLinkable) {
    override def dependentApply(row: Int, col: Int): A = {
      val (thisRow, thatRow) = (row / that.rows, row % that.rows)
      val (thisCol, thatCol) = (col / that.cols, col % that.cols)
      r.times(underingly1(thisRow, thisCol), underingly2(thatRow, thatCol))
    }
  }

  def +(that: Matrix[A])(implicit sg: Semigroup[A]): Matrix[A] = {
    checkSameSize(this, that)
    val buf = Array.ofDim[A](numel)
    var ind = buf.length - 1
    while (ind >= 0) {
      buf(ind) = sg.plus(this(ind), that(ind))
      ind -= 1
    }
    new DirectMatrix(rows, cols, buf)
  }

  def *(that: Matrix[A])(implicit r: Ring[A]): Matrix[A] = {
    if (this.cols != that.rows)
      throw new MatrixSizeMatchError("Matrix size must agree.")

    val buf = Array.ofDim[A](this.rows * that.cols)
    var ind = buf.length - 1
    while (ind >= 0) {
      val row = ind % this.rows
      val col = ind / this.rows
      var idx = this.cols - 1
      var res = r.zero
      while (idx >= 0) {
        res = r.plus(res, r.times(this(row, idx), that(idx, col)))
        idx -= 1
      }
      buf(ind) = res
      ind -= 1
    }
    new DirectMatrix(this.rows, that.cols, buf)
  }
  
  def +(that: A)(implicit sg: Semigroup[A]): Matrix[A] = {
    new DirectMatrix(rows, cols, buffer.map(x => sg.plus(x, that)))
  }
  def *(that: A)(implicit r: Ring[A]): Matrix[A] = {
    new DirectMatrix(rows, cols, buffer.map(x => r.times(x, that)))
  }
  
  def >(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL -1) >> LogWL)
    var ind = numel - 1
    while(ind >= 0) {
      if(ord.gt(buffer(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  
  def >=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL -1) >> LogWL)
    var ind = numel - 1
    while(ind >= 0) {
      if(ord.gteq(buffer(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  
  def <(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL -1) >> LogWL)
    var ind = numel - 1
    while(ind >= 0) {
      if(ord.lt(buffer(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  
  def <=(that: A)(implicit ord: Ordering[A]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL -1) >> LogWL)
    var ind = numel - 1
    while(ind >= 0) {
      if(ord.lteq(buffer(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }

  override def clone: Matrix[A] = new SingleDependentMatrix[A](rows, cols, this) {
    @inline override def dependentApply(row: Int, col: Int): A = underingly(row, col)
    @inline override def dependentApply(ind: Int): A = underingly(ind)
  }

  def reified(): Matrix[A] = this

  protected def checkSameSize(a: Matrix[_], b: Matrix[_]) {
    if (a.rows != b.rows || a.cols != b.cols)
      throw new MatrixSizeMatchError("Matrix size must agree.")
  }
}