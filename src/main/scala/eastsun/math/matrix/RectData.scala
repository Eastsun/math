package eastsun.math.matrix

import scala.reflect.ClassTag

trait RectData[A] {

  def rows: Int
  def cols: Int
  def numel: Int
  def size: (Int, Int)

  def apply(ind: Int): A
  def apply(row: Int, col: Int): A
  def iterator: Iterator[A]

  override def toString: String = {
    if (numel > 10000) {
      return "Matrix(%d * %d)".format(rows, cols)
    }

    val sb = new StringBuilder()
    if (iterator.map(_.toString).forall(_.count('.'==) == 1)) {
      val (pLen, sLen) = Array.tabulate(cols) { col =>
        Iterator.tabulate(rows)(row => apply(row, col).toString.span('.' !=))
        .foldLeft((0, 0)){ (r, s) =>
          (r._1 max s._1.size, r._2 max s._2.size)
        }
      }.unzip
      for (row <- 0 until rows; col <- 0 until cols) {
        val (pre, suf) = apply(row, col).toString.span('.' !=)
        sb ++= " " * (pLen(col) - pre.size) + pre + suf + " " * (sLen(col) - suf.size)
        sb ++= (if (col == cols - 1) ";\n" else ", ")
      }
    } else {
      val len = Array.tabulate(cols) { col =>
        Iterator.tabulate(rows)(row => apply(row, col).toString.size).max
      }
      for (row <- 0 until rows; col <- 0 until cols) {
        val str = apply(row, col).toString
        sb ++= " " * (len(col) - str.length) + str
        sb ++= (if (col == cols - 1) ";\n" else ", ")
      }
    }
    sb.toString
  }
}

object RectData {

  def tabulate[A: ClassTag](rows: Int, cols: Int)(f: (Int, Int) => A): RectData[A] =
    new AbstractRectData[A](rows, cols) {
      private[this] val array = Array.tabulate(rows, cols)(f)
      override def apply(row: Int, col: Int): A = array(row)(col)
    }

  def fill[A](rows: Int, cols: Int)(elem: A): RectData[A] = new AbstractRectData[A](rows, cols) {
    override def apply(ind: Int): A = elem
    override def apply(row: Int, col: Int): A = elem
    override def iterator: Iterator[A] = Iterator.fill(numel)(elem)
  }
}

private[matrix] abstract class AbstractRectData[A](val rows: Int, val cols: Int) extends RectData[A] {
  def numel: Int = rows * cols
  def size: (Int, Int) = (rows, cols)
  def apply(ind: Int): A = apply(ind % rows, ind / rows)
  def apply(row: Int, col: Int): A = apply(row + col * rows)
  def iterator: Iterator[A] = Iterator.tabulate(numel)(i => apply(i))
}