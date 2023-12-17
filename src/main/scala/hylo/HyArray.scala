package hylo

import java.util.Arrays

/** An ordered, random-access collection. */
final class HyArray[Element] private (
    private val _storage: scala.Array[AnyRef | Null] | Null,
    private var _count: Int // NOTE: where do I document private fields
) {

  // NOTE: The fact that we need Array[AnyRef] is diappointing and difficult to discover
  // The compiler error sent me on a wild goose chase with ClassTag.

  /** Returns `true` iff `this` is empty. */
  def isEmpty: Boolean =
    _count == 0

  /** Returns the number of elements in `this`. */
  def count: Int =
    _count

  /** Returns the number of elements that `this` can contain before allocating new storage. */
  def capacity: Int =
    if (_storage == null) { 0 }
    else { _storage.length }

  /** Reserves enough storage to store `n` elements in `this`. */
  def reserve_capacity(n: Int): HyArray[Element] =
    if (n <= capacity) {
      this
    } else {
      var newCapacity = max(1, capacity)
      while (newCapacity < n) { newCapacity = newCapacity << 1 }

      val newStorage = new scala.Array[AnyRef | Null](newCapacity)
      val s          = _storage.asInstanceOf[scala.Array[AnyRef | Null]]
      var i          = 0
      while (i < count) {
        newStorage(i) = _storage(i)
        i += 1
      }
      new HyArray(newStorage, count)
    }

  /** Adds a new element at the end of the array. */
  def append(source: Element): HyArray[Element] =
    val clone = copy(capacity + 1)
    clone._storage(count) = source.asInstanceOf[AnyRef]
    clone._count += 1
    clone

  /** Adds the contents of `source` at the end of the array. */
  def appendContents[C: Collection](source: C): HyArray[Element] =
    val clone = copy(count + source.count)
    this.reduce(clone, (r, e) => r.append(e))

  /** Removes and returns the last element, or returns `None` if the array is empty. */
  def popLast(): (HyArray[Element], Option[Element]) =
    if (isEmpty) {
      (this, None)
    } else {
      val clone = copy()
      clone._count -= 1
      (clone, Some(clone._storage(clone._count).asInstanceOf[Element]))
    }

  /** Removes all elements in the array, keeping allocated storage iff `keepStorage` is true. */
  def removeAll(keepStorage: Boolean = false): HyArray[Element] =
    if (isEmpty) {
      this
    } else if (keepStorage) {
      val clone = copy()
      Arrays.fill(clone._storage, null)
      clone._count = 0
      clone
    } else {
      HyArray()
    }

  /** Accesses the element at `p`.
    *
    * @requires
    *   `p` is a valid position in `self` different from `endPosition`.
    * @complexity
    *   O(1).
    */
  def at(p: Int): Element =
    _storage(p).asInstanceOf[Element]

  /** Returns a textual description of `this`. */
  override def toString: String =
    var s = "["
    var i = 0
    while (i < count) {
      if (i > 0) { s += ", " }
      s += s"${at(i)}"
      i += 1
    }
    s + "]"

  /** Returns an independent copy of `this`. */
  private def copy(minimumCapacity: Int = 0): HyArray[Element] =
    if (minimumCapacity > capacity) {
      // If the requested capacity on the copy is greater than what we have, `reserveCapacity` will
      // create an independent value.
      reserve_capacity(minimumCapacity)
    } else {
      val clone = HyArray[Element]().reserve_capacity(max(minimumCapacity, count))
      var i     = 0
      while (i < count) {
        clone._storage(i) = _storage(i)
        i += 1
      }
      clone._count = count
      clone
    }

}

object HyArray {

  /** Creates an array with the given `elements`. */
  def apply[T](elements: T*): HyArray[T] =
    var a = new HyArray[T](null, 0)
    for (e <- elements) a = a.append(e)
    a

}

given hyArrayIsCollection[T]: Collection[HyArray[T]] with {

  type Element = T

  type Position = Int
  given positionIsEquatable: Equatable[Int] = intIsComparable

  extension (self: HyArray[T]) {

    // NOTE: Having to explicitly override means that primary declaration can't automatically
    // specialize trait requirements.
    override def isEmpty: Boolean = self.isEmpty

    override def count: Int = self.count

    def startPosition = 0

    def endPosition = self.count

    def positionAfter(p: Int) = p + 1

    def at(p: Int) = self.at(p)

  }

}
