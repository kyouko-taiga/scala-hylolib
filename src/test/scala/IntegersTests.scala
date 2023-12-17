import hylo.*
import hylo.given

class IntegersTests extends munit.FunSuite {

  test("Int.hashInto") {
    val h = Hasher()
    val x = 42.hashInto(h)
    val y = 42.hashInto(h)
    assertEquals(x, y)
  }

}
