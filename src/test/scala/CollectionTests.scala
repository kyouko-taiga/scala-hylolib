import hylo.*
import hylo.given

class CollectionTests extends munit.FunSuite {

  test("isEmpty") {
    // NOTE: Passing the evidence implicitly disables parts of the API.
    val empty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int]())
    assert(empty.isEmpty)

    // NOTE: Eliding the generic argument to `HyArray` causes a type error.
    val nonEmpty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2))
    assert(!nonEmpty.isEmpty)
  }

  test("count") {
    val a = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2))
    assertEquals(a.count, 2)
  }

  test("isBefore") {
    val empty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int]())
    assert(!empty.isBefore(empty.startPosition, empty.endPosition))

    val nonEmpty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2))
    val p0 = nonEmpty.startPosition
    val p1 = nonEmpty.positionAfter(p0)
    val p2 = nonEmpty.positionAfter(p1)
    assert(nonEmpty.isBefore(p0, nonEmpty.endPosition))
    assert(nonEmpty.isBefore(p1, nonEmpty.endPosition))
    assert(!nonEmpty.isBefore(p2, nonEmpty.endPosition))
  }

  test("headAndTail") {
    val empty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int]())
    assertEquals(empty.headAndTail, None)

    val one = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1))
    val Some((h0, t0)) = one.headAndTail: @unchecked
    assert(h0 eq 1)
    assert(t0.isEmpty)

    val two = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2))
    val Some((h1, t1)) = two.headAndTail: @unchecked
    assertEquals(h1, 1)
    assertEquals(t1.count, 1)
  }

  test("reduce") {
    val empty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int]())
    assertEquals(empty.reduce(0, (s, x) => s + x), 0)

    val nonEmpty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2, 3))
    assertEquals(nonEmpty.reduce(0, (s, x) => s + x), 6)
  }

  test("forEach") {
    val empty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int]())
    assert(empty.forEach((e) => false))

    val nonEmpty = AnyCollection(using hyArrayIsCollection[Int])(HyArray[Int](1, 2, 3))
    var s = 0
    assert(nonEmpty.forEach((e) => { s += e; true }))
    assertEquals(s, 6)

    s = 0
    assert(!nonEmpty.forEach((e) => { s += e; false }))
    assertEquals(s, 1)
  }

  // NOTE: Couldn't find a workaround for the ambiguous given instances.
  // test("elementsEqual") {
  //   val a = HyArray(1, 2)
  //   assert(a.elementsEqual(a))
  // }

}
