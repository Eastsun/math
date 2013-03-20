package eastsun.math.matrix

import org.scalatest.FunSuite
import Matrix._

class ExampleSuite extends FunSuite {

  test("Matrix indexing") {
    val mat = rand[Int](3, 4).map(_ % 10)
    val neg = mat >= 0
    val pos = mat < 0
    val nat = Matrix.zero[Int](3, 4)
    nat(neg) = mat(neg) * -1
    nat(pos) = mat(pos) * -1
    val sum = mat + nat
    assert(sum.iterator.forall(0==))
  }
}