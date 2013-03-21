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

    val subMatrix = m(0 to end by 3, 0 to end by 3)
    assert(subMatrix === Matrix(3, 3, 1::4::7::28::31::34::55::58::61::Nil))
  }

  test("Matrix plus operate") {
    val m1 = rand[Byte](3, 4).map(_%10)
    val m2 = rand[Byte](3, 4).map(_%10)
    val m3 = m1 + m2
    val m4 = m2 + m1
    m1 += m2
    assert(m1 === m3)
    assert(m1 === m4)

    val b1 = rand[Boolean](2, 5)
    val b2 = rand[Boolean](2, 5)
    val b3 = b1 + b2
    val b4 = b2 + b1
    b1 += b2
    assert(b1 === b3)
    assert(b1 === b4)
  }

  test("Matrix multiply operate") {
    val n1 = rand[Int](3, 4).map(_%10)
    val n2 = rand[Int](4, 3).map(_%10)

    val m1 = n1 * n2
    val m2 = n2 * n1
    val trace1 = (0 until m1.rows).map(i => m1(i,i)).sum
    val trace2 = (0 until m2.rows).map(i => m2(i,i)).sum
    assert(trace1 === trace2)
  }

  test("Matrix update operate") {
    val m = rand[Int](3, 4).map(_%10)
    m(all, 0) = 0
    val firstCol = m(all, 0)
    assert(firstCol.iterator.forall(_==0))
    m(0, all) = 1
    val firstRow = m(0, all)
    assert(firstRow.iterator.forall(_==1))

    val m2 = rand[Int](3, 5).map(_%10)
    val negative = m2(m2 < 0)
    assert(negative.iterator.forall(_ < 0))
  }
}