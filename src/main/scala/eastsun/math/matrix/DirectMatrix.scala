package eastsun.math.matrix

import scala.reflect.ClassTag

private[matrix] class DirectMatrix[A: ClassTag](nRows: Int, nCols: Int, buf: Array[A]) 
      	extends ArrayMatrix[A](nRows, nCols, buf) {
}