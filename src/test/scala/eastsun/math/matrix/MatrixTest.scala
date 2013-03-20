package eastsun.math.matrix

import org.scalatest.FunSuite
import Matrix._

class ExampleSuite extends FunSuite {

  test("Matrix indexing") {
    val m = Matrix(9, 9, 1 to 81)

    val endOfFirstRow = m(0, end)
    val endOfFirstCol = m(end, 0)

    assert(endOfFirstRow === 73)
    assert(endOfFirstCol === 9)

    val firstRow = m(0, all)
    val firstCol = m(all, 0)
    assert(firstRow === Matrix(1, 9, 1 to 73 by 9))
    assert(firstCol === Matrix(9, 1, 1 to 9))

    val allElements = m(all)
    assert(allElements === Matrix(81, 1, 1 to 81))
  }
}