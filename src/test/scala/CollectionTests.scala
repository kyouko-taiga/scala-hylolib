import hylo.*
import hylo.given

class CollectionTests extends munit.FunSuite {

  test("isEmpty") {
    // NOTE: Not providing the causes a type error
    // Type Error:
    //   Ambiguous given instances: both given instance anyCollectionIsCollection in package hylo
    //   and given instance hyArrayIsCollection in package hylo match type hylo.Collection[Base]
    //   of parameter b of method apply in object AnyCollection
    val empty = AnyCollection(using hyArrayIsCollection)(HyArray[Int]())
    assert(empty.isEmpty)

    val nonEmpty = AnyCollection(using hyArrayIsCollection)(HyArray[Int](1, 2))
    assert(!nonEmpty.isEmpty)
  }

  test("count") {
    val a = AnyCollection(using hyArrayIsCollection)(HyArray[Int](1, 2))
    assertEquals(a.count, 2)
  }

  test("isBefore") {
    val empty = AnyCollection(using hyArrayIsCollection)(HyArray[Int]())
    assert(!empty.isBefore(empty.startPosition, empty.endPosition))

    val nonEmpty = AnyCollection(using hyArrayIsCollection)(HyArray[Int](1, 2))
    val p0 = nonEmpty.startPosition
    val p1 = nonEmpty.positionAfter(p0)
    val p2 = nonEmpty.positionAfter(p1)
    assert(nonEmpty.isBefore(p0, nonEmpty.endPosition))
    assert(nonEmpty.isBefore(p1, nonEmpty.endPosition))
    assert(!nonEmpty.isBefore(p2, nonEmpty.endPosition))
  }

  // NOTE: Couldn't find a workaround for the ambiguous given instances.
  // test("elementsEqual") {
  //   val a = HyArray(1, 2)
  //   assert(a.elementsEqual(a))
  // }

}
