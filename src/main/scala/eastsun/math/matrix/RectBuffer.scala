package eastsun.math.matrix

trait RectBuffer[A] extends RectData[A]  {

  def update(ind: Int, value: A): Unit
  
  def update(row: Int, col: Int, value: A): Unit
}

private[matrix] class Array2[A](nRows: Int, nCols: Int, array: Array[Array[A]]) 
                 extends AbstractRectData[A](nRows, nCols) with RectBuffer[A]{
  @inline override def apply(ind: Int): A = array(ind%rows)(ind/rows)
  @inline override def apply(row: Int, col: Int): A = array(row)(col)
  @inline def update(ind: Int, value: A): Unit = array(ind%rows)(ind/rows) = value
  @inline def update(row: Int, col: Int, value: A): Unit = array(row)(col) = value
}