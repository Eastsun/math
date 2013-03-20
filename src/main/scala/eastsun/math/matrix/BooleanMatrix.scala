package eastsun.math.matrix

import scala.reflect.ClassTag
import scala.ref.WeakReference
import scala.collection.mutable
import eastsun.math.algebra.Semigroup
import eastsun.math.algebra.Ring
import eastsun.math.algebra.Field
import BooleanMatrix._

private[matrix] object BooleanMatrix {
  val LogWL = 6
  val WordL = 64
}

private[matrix] class BooleanMatrix(val rows: Int, val cols: Int, protected val bits: Array[Long])
  extends LinkableRectData[Boolean] with Matrix[Boolean] {

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

  val length: Int = math.max(rows, cols)
  val numel: Int = rows * cols
  def size: (Int, Int) = (rows, cols)

  @inline def apply(ind: Int): Boolean = (bits(ind >> LogWL) & (1L << ind)) != 0
  @inline def apply(ind: Index): Boolean = apply(ind.atBound(numel))
  @inline def apply(row: Int, col: Int): Boolean = apply(row + col * rows)
  def apply(row: Int, col: Index): Boolean = apply(row, col.atBound(cols))
  def apply(row: Index, col: Int): Boolean = apply(row.atBound(rows), col)
  def apply(row: Index, col: Index): Boolean = apply(row.atBound(rows), col.atBound(cols))
  def apply[Ind](ind: Ind)(implicit i: Indexed[Ind]): Matrix[Boolean] = {
    val ii = i.atBound(numel, ind)
    val buffer = Array.ofDim[Long]((ii.numel + WordL - 1) >> LogWL)
    var idx = ii.numel - 1
    while (idx >= 0) {
      if (apply(ii(idx))) buffer(idx >> LogWL) |= 1L << idx
      idx -= 1
    }
    new BooleanMatrix(ii.rows, ii.cols, buffer)
  }
  def apply[Row, Col](row: Row, col: Col)(implicit r: Indexed[Row], c: Indexed[Col]): Matrix[Boolean] = {
    val ri = r.atBound(rows, row)
    val ci = c.atBound(cols, col)
    val num = ri.numel * ci.numel
    val buffer = Array.ofDim[Long]((num + WordL - 1) >> LogWL)
    var rIdx = ri.numel - 1
    while (rIdx >= 0) {
      var cIdx = ci.numel - 1
      while (cIdx >= 0) {
        val idx = rIdx + cIdx * ri.numel
        if (apply(ri(rIdx), ci(cIdx))) buffer(idx >> LogWL) |= 1L << idx
        cIdx -= 1
      }
      rIdx -= 1
    }
    new BooleanMatrix(ri.numel, ci.numel, buffer)
  }

  @inline def update0(ind: Int, value: Boolean): Unit = {
    if (value) bits(ind >> LogWL) |= 1L << ind
    else bits(ind >> LogWL) &= ~(1L << ind)
  }
  @inline def update(ind: Int, value: Boolean): Unit = {
    reifyLinks()
    update0(ind, value)
  }
  @inline def update(ind: Index, value: Boolean): Unit = update(ind.atBound(numel), value)
  @inline def update(row: Int, col: Int, value: Boolean): Unit = update(row + col * rows, value)
  @inline def update(row: Int, col: Index, value: Boolean): Unit = update(row, col.atBound(cols), value)
  @inline def update(row: Index, col: Int, value: Boolean): Unit = update(row.atBound(rows), col, value)
  @inline def update(row: Index, col: Index, value: Boolean): Unit = update(row.atBound(rows), col.atBound(cols), value)
  def update[Ind, V](ind: Ind, value: V)(implicit i: Indexed[Ind], v: Valued[Boolean, V]): Unit = {
    reifyLinks()
    val ii = i.atBound(numel, ind)
    val vv = v.fitSize(ii.numel, value)
    var idx = ii.numel - 1
    while (idx >= 0) {
      update0(ii(idx), vv(idx))
      idx -= 1
    }
  }
  def update[Row, Col, V](row: Row, col: Col, value: V)(implicit r: Indexed[Row], c: Indexed[Col], v: Valued[Boolean, V]): Unit = {
    reifyLinks()
    val ri = r.atBound(rows, row)
    val ci = c.atBound(cols, col)
    val vv = v.fitSize(ri.numel, ci.numel, value)
    var rIdx = ri.numel - 1
    while (rIdx >= 0) {
      var cIdx = ci.numel - 1
      while (cIdx >= 0) {
        update0(ri(rIdx) + ci(cIdx) * rows, vv(rIdx, cIdx))
        cIdx -= 1
      }
      rIdx -= 1
    }
  }

  def map[B](f: Boolean => B)(implicit ct: ClassTag[B]): Matrix[B] = ct.toString match {
    case "Boolean" =>
      val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
      val g = f.asInstanceOf[Boolean => Boolean]
      var ind = numel - 1
      while (ind >= 0) {
        if (g(apply(ind))) buf(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      (new BooleanMatrix(rows, cols, buf)).asInstanceOf[Matrix[B]]
    case _ =>
      new DirectMatrix(rows, cols, Array.tabulate(numel)(ind => f(apply(ind))))
  }

  def >(that: Boolean)(implicit ord: Ordering[Boolean]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
    var ind = numel - 1
    while (ind >= 0) {
      if (ord.gt(this(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  def >=(that: Boolean)(implicit ord: Ordering[Boolean]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
    var ind = numel - 1
    while (ind >= 0) {
      if (ord.gteq(this(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  
  def <(that: Boolean)(implicit ord: Ordering[Boolean]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
    var ind = numel - 1
    while (ind >= 0) {
      if (ord.lt(this(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }
  def <=(that: Boolean)(implicit ord: Ordering[Boolean]): Matrix[Boolean] = {
    val buf = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
    var ind = numel - 1
    while (ind >= 0) {
      if (ord.lteq(this(ind), that)) buf(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(rows, cols, buf)
  }

  def unzip[T1, T2](implicit isPair: Boolean =:= (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Matrix[T1], Matrix[T2]) = ???

  protected[matrix] def asLinkable: LinkableRectData[Boolean] = this

  def transpose: Matrix[Boolean] = {
    val buffer = Array.ofDim[Long]((numel + WordL - 1) >> LogWL)
    var ind = numel - 1
    while (ind >= 0) {
      if (apply(ind / cols, ind % cols))
        buffer(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(cols, rows, buffer)
  }
  def reshape(nRows: Int, nCols: Int): Matrix[Boolean] =
    if (nRows * nCols != numel)
      throw new MatrixSizeMatchError("To RESHAPE the number of elements must not change.")
    else new BooleanMatrix(nRows, nCols, bits.clone)

  override def clone: Matrix[Boolean] = new BooleanMatrix(rows, cols, bits.clone)

  def reified(): Matrix[Boolean] = this

  def +(that: Matrix[Boolean])(implicit sg: Semigroup[Boolean]): Matrix[Boolean] = {
    checkSameSize(this, that)
    val buffer = Array.ofDim[Long](bits.length)
    if ((sg eq Field.BooleanIsField) && that.isInstanceOf[BooleanMatrix]) {
      val tBits = that.asInstanceOf[BooleanMatrix].bits
      var ind = bits.length - 1
      while (ind >= 0) {
        buffer(ind) = bits(ind) ^ tBits(ind)
        ind -= 1
      }
    } else {
      var ind = numel - 1
      while (ind >= 0) {
        if (sg.plus(this(ind), that(ind)))
          buffer(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
    }
    new BooleanMatrix(rows, cols, buffer)
  }

  def *(that: Matrix[Boolean])(implicit r: Ring[Boolean]): Matrix[Boolean] = {
    if (this.cols != that.rows)
      throw new MatrixSizeMatchError("Matrix size must agree.")

    val num = this.rows * that.cols
    val buffer = Array.ofDim[Long]((num + WordL - 1) >> LogWL)
    var ind = num - 1
    while (ind >= 0) {
      val (rIdx, cIdx) = (ind % rows, ind / rows)
      var res = r.zero
      var idx = cols - 1
      while (idx >= 0) {
        res = r.plus(res, r.times(this(rIdx, idx), that(idx, cIdx)))
        idx -= 1
      }
      if (res)
        buffer(ind >> LogWL) |= 1L << ind
      ind -= 1
    }
    new BooleanMatrix(this.rows, that.cols, buffer)
  }

  def +(that: Boolean)(implicit sg: Semigroup[Boolean]): Matrix[Boolean] = {
    if (sg eq Field.BooleanIsField) {
      if (that) {
        val buffer = Array.ofDim[Long](bits.length)
        var ind = bits.length - 1
        while (ind >= 0) {
          buffer(ind) = ~bits(ind)
          ind -= 1
        }
        new BooleanMatrix(rows, cols, buffer)
      } else this.clone
    } else {
      val buffer = Array.ofDim[Long](bits.length)
      var ind = numel - 1
      while (ind >= 0) {
        if (sg.plus(this(ind), that))
          buffer(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buffer)
    }
  }

  def *(that: Boolean)(implicit r: Ring[Boolean]): Matrix[Boolean] = {
    if (r eq Field.BooleanIsField) {
      if (that) this.clone
      else {
        val buffer = Array.ofDim[Long](bits.length)
        new BooleanMatrix(rows, cols, buffer)
      }
    } else {
      val buffer = Array.ofDim[Long](bits.length)
      var ind = numel - 1
      while (ind >= 0) {
        if (r.times(this(ind), that))
          buffer(ind >> LogWL) |= 1L << ind
        ind -= 1
      }
      new BooleanMatrix(rows, cols, buffer)
    }
  }

  def kron(that: Matrix[Boolean])(implicit r: Ring[Boolean]): Matrix[Boolean] = {
    val num = rows * cols * that.rows * that.cols
    val buffer = Array.ofDim[Long]((num + WordL - 1) >> LogWL)
    var idx = numel - 1
    while (idx >= 0) {
      var jdx = that.numel - 1
      while (jdx >= 0) {
        val ind = (idx % rows * that.rows + jdx % that.rows) +
          (idx / rows * that.cols + jdx / that.rows) * rows * that.rows
        if (r.times(this(idx), that(jdx))) buffer(ind >> LogWL) |= 1L << ind
        jdx -= 1
      }
      idx -= 1
    }
    new BooleanMatrix(rows * that.rows, cols * that.cols, buffer)
  }

  override def iterator: Iterator[Boolean] = Iterator.tabulate(numel)(ind => (bits(ind >> LogWL) & (1L << ind)) != 0)

  override def toString: String = {
    val sb = new StringBuilder
    for (row <- 0 until rows; col <- 0 until cols) {
      sb ++= (if (this(row, col)) "1" else "0")
      sb ++= (if (col == cols - 1) ";\n" else ", ")
    }
    sb.toString
  }

  protected def checkSameSize(a: Matrix[_], b: Matrix[_]) {
    if (a.rows != b.rows || a.cols != b.cols)
      throw new MatrixSizeMatchError("Matrix size must agree.")
  }
}
