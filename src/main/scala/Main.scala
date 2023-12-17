import hylo.*
import hylo.given

@main def hello: Unit =
  var a = HyArray[Int]()
  a = a.append(0)
  a = a.append(1)
  a = a.append(2)

  val Some((_, tail)) = a.headAndTail: @unchecked

  val x = tail.minElement((a, b) => a.asInstanceOf[Int] < b.asInstanceOf[Int])
  assert(x.get.asInstanceOf[Int] eq 1)

  // NOTE: Not providing the evidence explicitly produces a strange error:
  // Type Mismatch Error:
  //   Found:    (tail : hylo.Slice[hylo.HyArray[Int]])
  //   Required: hylo.HyArray[T]
  val u = AnyCollection(using sliceIsCollection[HyArray[Int]])(tail)

  // NOTE: The compiler should conclude that `tail.Element` is `Int`.
  // tail.minElement((a, b) => a < b)

  // NOTE: It should be possible to call `minElement` because `Int` models `Comparable`
  // tail.minElement()

  val b = AnyCollection(using hyArrayIsCollection)(HyArray[Int](1, 2))
  println(b.count)
