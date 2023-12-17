import hylo.*
import hylo.given

class HyArrayTests extends munit.FunSuite {

  test("reserveCapacity") {
    var a = HyArray[Int]()
    a = a.append(1)
    a = a.append(2)

    a = a.reserve_capacity(10)
    assert(a.capacity >= 10)
    assertEquals(a.count, 2)
    assertEquals(a.at(0), 1)
    assertEquals(a.at(1), 2)
  }

}
