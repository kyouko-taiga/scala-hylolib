import hylo.*
import hylo.given

class AnyEquatableTests extends munit.FunSuite {

  test("eq") {
    val a = AnyEquatable(1)
    assert(a eq a)
    assert(!(a neq a))

    val b = AnyEquatable(2)
    assert(!(a eq b))
    assert(a neq b)
  }

}
