package eastsun.math.matrix

import scala.collection.mutable
import scala.ref.WeakReference

trait LinkableRectData[A] extends RectData[A] {
  
  def linkBy(m: Matrix[_]): Unit = {}

  def unlink(m: Matrix[_]): Unit = {}

  def reifyLinks(): Unit ={}
}

object LinkableRectData {
  def fill[A](rows: Int, cols: Int)(elem: A): LinkableRectData[A] = new AbstractRectData[A](rows, cols) with LinkableRectData[A] {
    override def apply(ind: Int): A = elem
    override def apply(row: Int, col: Int): A = elem
    override def iterator: Iterator[A] = Iterator.fill(numel)(elem)
  }
}