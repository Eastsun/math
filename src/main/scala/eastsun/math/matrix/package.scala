package eastsun.math

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound

package object matrix {

  type Indices = RectData[Int]
  type Values[A] = RectData[A]
  val end = EndIndex()
  val all = 0 to end

  implicit class ZipOps[T1, T2](val tup: (Matrix[T1], Matrix[T2])) extends AnyVal {
    def zip(implicit ct: ClassTag[(T1, T2)]): Matrix[(T1, T2)] =
      new TupleMatrix[T1, T2](tup._1, tup._2)
  }

  implicit class ZipOps3[T1, T2, T3](val tri: (Matrix[T1], Matrix[T2], Matrix[T3])) extends AnyVal {
    def zip(implicit ct: ClassTag[(T1, T2, T3)]): Matrix[(T1, T2, T3)] =
      new TripleMatrix[T1, T2, T3](tri._1, tri._2, tri._3)
  }

  implicit class Int2EndRange(val num: Int) extends AnyVal {
    def to(value: EndIndex) = new EndedRangeWithBy(Left(num), Right(value), 1)
  }
}

package matrix {

  class MatrixSizeMatchError(msg: String) extends RuntimeException(msg)

  trait Index {
    def atBound(b: Int): Int
  }
  trait Indexed[-I] {
    def atBound(bound: Int, index: I): Indices
    def atBound(rows: Int, cols: Int, index: I): Indices = atBound(rows * cols, index)
  }

  @implicitNotFound(msg = "Value of type ${V} can't be assigned to Matrix[${A}]")
  trait Valued[A, -V] {
    def fitSize(rows: Int, cols: Int, underingly: V): Values[A]
    def fitSize(numel: Int, underingly: V): Values[A]
  }

  object Valued {
    implicit def rectDataIsValued[A]: Valued[A, RectData[A]] = new Valued[A, RectData[A]] {
      def fitSize(rows: Int, cols: Int, underingly: RectData[A]): Values[A] = {
        checkSize((rows == underingly.rows && cols == underingly.cols) ||
          (((rows == 1 || cols == 1) && (underingly.rows == 1 || underingly.cols == 1)) &&
            rows * cols == underingly.numel))
        underingly
      }

      def fitSize(numel: Int, underingly: RectData[A]): Values[A] = {
        checkNumel(numel == underingly.numel)
        underingly
      }
    }

    implicit def singleValueIsValued[A]: Valued[A, A] = new Valued[A, A] {
      def fitSize(rows: Int, cols: Int, underingly: A): Values[A] = RectData.fill(rows, cols)(underingly)
      def fitSize(numel: Int, underingly: A): Values[A] = RectData.fill(1, numel)(underingly)
    }

    implicit def seqIsValued[A, Coll <% Seq[A]]: Valued[A, Coll] = new Valued[A, Coll] {
      def fitSize(rows: Int, cols: Int, underingly: Coll): Values[A] = {
        checkSize((rows == 1 || cols == 1) && rows * cols == underingly.size)
        new SeqValues(underingly)
      }

      def fitSize(numel: Int, underingly: Coll): Values[A] = {
        checkNumel(numel == underingly.size)
        new SeqValues(underingly)
      }
      class SeqValues(seq: Seq[A]) extends AbstractRectData[A](1, seq.size) {
        @inline override def apply(ind: Int): A = seq(ind)
        @inline override def iterator: Iterator[A] = seq.iterator
      }
    }
    private def checkNumel(condition: Boolean) {
      if (!condition)
        throw new MatrixSizeMatchError("In an assignment  A(I) = B, the number of elements in B and I must be the same.")
    }
    private def checkSize(condition: Boolean) {
      if (!condition)
        throw new MatrixSizeMatchError("Subscripted assignment dimension mismatch.")
    }
  }

  object Indexed {
    implicit object EndIndexIsIndexed extends Indexed[EndIndex] {
      def atBound(bound: Int, index: EndIndex): Indices = new AbstractRectData[Int](1, 1) {
        val i = index.atBound(bound)
        @inline override def apply(ind: Int): Int = i
        @inline override def apply(row: Int, col: Int): Int = i
      }
    }
    implicit object IntIsIndexed extends Indexed[Int] {
      def atBound(bound: Int, index: Int): Indices = new AbstractRectData[Int](1, 1) {
        @inline override def apply(ind: Int): Int = index
        @inline override def apply(row: Int, col: Int): Int = index
      }
    }

    implicit object IntSeqIsIndexed extends Indexed[Seq[Int]] {
      def atBound(bound: Int, index: Seq[Int]): Indices = new AbstractRectData[Int](1, index.size) {
        val indSeq = index.toIndexedSeq
        @inline override def apply(ind: Int): Int = indSeq(ind)
        @inline override def apply(row: Int, col: Int): Int = indSeq(col)
      }
    }

    implicit object IntArrayIsIndexed extends Indexed[Array[Int]] {
      def atBound(bound: Int, index: Array[Int]): Indices = new AbstractRectData[Int](1, index.length) {
        val indSeq = index.clone
        @inline override def apply(ind: Int): Int = indSeq(ind)
        @inline override def apply(row: Int, col: Int): Int = indSeq(col)
      }
    }

    implicit object IntMatrixIsIndexed extends Indexed[Matrix[Int]] {
      def atBound(bound: Int, index: Matrix[Int]): Indices = index.clone
    }

    implicit object BooleanMatrixIsIndexed extends Indexed[Matrix[Boolean]] {
      def atBound(bound: Int, index: Matrix[Boolean]): Indices = {
        val indSeq = (0 until index.numel).filter(ind => index(ind))
        new AbstractRectData[Int](indSeq.size, 1) {
          @inline override def apply(ind: Int): Int = indSeq(ind)
          @inline override def apply(row: Int, col: Int): Int = indSeq(row)
        }
      }
      
      override def atBound(rows: Int, cols: Int, index: Matrix[Boolean]): Indices = {
        if(index.rows > rows || index.cols > cols)
          throw new MatrixSizeMatchError("Index exceeds matrix dimensions.")
        val indSeq = for(ind <- 0 until index.numel; if index(ind)) yield ind%index.rows + ind/index.rows*rows 
        new AbstractRectData[Int](indSeq.size, 1) {
          @inline override def apply(ind: Int): Int = indSeq(ind)
          @inline override def apply(row: Int, col: Int): Int = indSeq(row)
        }
      }
    }

    implicit object EndedRangeIsIndexed extends Indexed[EndedRange] {
      def atBound(bound: Int, index: EndedRange): Indices = {
        val seq = index.toIndexedSeq(bound)
        new AbstractRectData[Int](1, seq.size) {
          @inline override def apply(ind: Int): Int = seq(ind)
          @inline override def apply(row: Int, col: Int): Int = seq(col)
        }
      }
    }
  }

  private[matrix] class EndIndex(val ops: List[(Char, Int)]) extends Index { self =>
    import EndIndex._
    def this() = this(Nil)

    def +(value: Int): EndIndex = EndIndex(('+', value) :: ops)
    def -(value: Int): EndIndex = EndIndex(('-', value) :: ops)
    def *(value: Int): EndIndex = EndIndex(('*', value) :: ops)
    def /(value: Int): EndIndex = EndIndex(('/', value) :: ops)

    def to(value: Int) = new EndedRangeWithBy(Right(self), Left(value), 1)
    def to(value: EndIndex) = new EndedRangeWithBy(Right(self), Right(value), 1)

    def atBound(b: Int): Int = ops.foldRight(b - 1)((op, r) => OP(op._1)(r, op._2))
    override def toString: String = ops.foldRight(("end", 4)) { (op, r) =>
      val (sym, num) = op
      val (str, pre) = r
      if (pre < W(sym)) ("(%s)%c%d".format(str, sym, num), W(sym))
      else (str + sym + num, W(sym))
    }._1
  }

  private[matrix] object EndIndex {
    def apply(ops: List[(Char, Int)]): EndIndex = new EndIndex(ops)
    def apply(): EndIndex = new EndIndex()
    val OP = Map[Char, (Int, Int) => Int]('+' -> (_ + _), '-' -> (_ - _), '*' -> (_ * _), '/' -> (_ / _))
    val W = Map[Char, Int]('+' -> 0, '-' -> 0, '*' -> 2, '/' -> 2)
  }

  private[matrix] class EndedRange(p1: Either[Int, EndIndex], p2: Either[Int, EndIndex], step: Int) {
    def toIndexedSeq(bound: Int): IndexedSeq[Int] =
      p1.fold(_.toInt, _.atBound(bound)) to p2.fold(_.toInt, _.atBound(bound)) by step
    override def toString: String =
      p1.fold(_.toString, _.toString) + " to " + p2.fold(_.toString, _.toString) + " by " + step
  }

  private[matrix] class EndedRangeWithBy(p1: Either[Int, EndIndex], p2: Either[Int, EndIndex], step: Int)
    extends EndedRange(p1, p2, step) {
    def by(step: Int): EndedRange = new EndedRange(p1, p2, step)
  }

  private[matrix] class TupleMatrix[T1, T2](m1: Matrix[T1], m2: Matrix[T2])
    extends DependentMatrix[(T1, T2)](m1.rows, m1.cols) {
    if (m1.size != m2.size)
      throw new MatrixSizeMatchError("Matrix size must agree.")

    var underingly1 = m1.asLinkable
    var underingly2 = m2.asLinkable

    underingly1.linkBy(TupleMatrix.this)
    underingly2.linkBy(TupleMatrix.this)

    override def releaseDependent(): Unit = {
      underingly1.unlink(TupleMatrix.this)
      underingly2.unlink(TupleMatrix.this)
      underingly1 = null
      underingly2 = null
    }

    @inline override def dependentApply(ind: Int): (T1, T2) = (underingly1(ind), underingly2(ind))
    @inline override def dependentApply(row: Int, col: Int): (T1, T2) = (underingly1(row, col), underingly2(row, col))
  }

  private[matrix] class TripleMatrix[T1, T2, T3](m1: Matrix[T1], m2: Matrix[T2], m3: Matrix[T3])
    extends DependentMatrix[(T1, T2, T3)](m1.rows, m1.cols) {
    if (m1.size != m2.size || m1.size != m3.size)
      throw new MatrixSizeMatchError("Matrix size must agree.")

    var underingly1 = m1.asLinkable
    var underingly2 = m2.asLinkable
    var underingly3 = m3.asLinkable

    underingly1.linkBy(this)
    underingly2.linkBy(this)
    underingly3.linkBy(this)

    override def releaseDependent(): Unit = {
      underingly1.unlink(this)
      underingly2.unlink(this)
      underingly3.unlink(this)
      underingly1 = null
      underingly2 = null
      underingly3 = null
    }

    @inline override def dependentApply(ind: Int): (T1, T2, T3) = (underingly1(ind), underingly2(ind), underingly3(ind))
    @inline override def dependentApply(row: Int, col: Int): (T1, T2, T3) =
      (underingly1(row, col), underingly2(row, col), underingly3(row, col))
  }
}




