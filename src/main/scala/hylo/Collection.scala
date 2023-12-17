package hylo

/** A collection of elements accessible by their position. */
trait Collection[Self] {

  /** The type of the elements in the collection. */
  type Element

  /** The type of a position in the collection. */
  type Position
  given positionIsEquatable: Equatable[Position]

  extension (self: Self) {

    /** Returns `true` iff `self` is empty. */
    def isEmpty: Boolean =
      startPosition eq endPosition

    /** Returns the number of elements in `self`.
      *
      * @complexity
      *   O(n) where n is the number of elements in `self`.
      */
    def count: Int =
      val e = endPosition
      def _count(p: Position, n: Int): Int =
        if (p eq e) { n }
        else { _count(self.positionAfter(p), n + 1) }
      _count(startPosition, 0)

    /** Returns the position of `self`'s first element', or `endPosition` if `self` is empty.
      *
      * @complexity
      *   O(1)
      */
    def startPosition: Position

    /** Returns the "past the end" position in `self`, that is, the position immediately after the
      * last element in `self`.
      *
      * @complexity
      *   O(1).
      */
    def endPosition: Position

    /** Returns the position immediately after `p`
      *
      * @requires
      *   `p` is a valid position in `self` different from `endPosition`.
      * @complexity
      *   O(1).
      */
    def positionAfter(p: Position): Position

    /** Accesses the element at `p`.
      *
      * @requires
      *   `p` is a valid position in `self` different from `endPosition`.
      * @complexity
      *   O(1).
      */
    def at(p: Position): Element

    /** Returns `true` iff `i` precedes `j`.
      *
      * @requires
      *   `i` and j` are valid positions in `self`.
      * @complexity
      *   O(n) where n is the number of elements in `self`.
      */
    def isBefore(i: Position, j: Position): Boolean =
      val e = self.endPosition
      if (i.eq(e)) {
        false
      } else if (j.eq(e)) {
        true
      } else {
        def _isBefore(n: Position): Boolean =
          if (n.eq(j)) {
            true
          } else if (n.eq(e)) {
            false
          } else {
            _isBefore(self.positionAfter(n))
          }
        _isBefore(self.positionAfter(i))
      }

  }

}

extension [Self](self: Self)(using s: Collection[Self]) {

  /** Returns the first element of `self` along with a slice containing the suffix after this
    * element, or `None` if `self` is empty.
    *
    * @complexity
    *   O(1)
    */
  def headAndTail: Option[(s.Element, Slice[Self])] =
    if (self.isEmpty) {
      None
    } else {
      val s = self.startPosition
      val t = self.positionAfter(s)
      Some((self.at(s), Slice(self, Range(t, self.endPosition, self.isBefore))))
    }

  /** Applies `combine` on `partialResult` and each element of `self`, in order.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def reduce[T](partialResult: T, combine: (T, s.Element) => T): T =
    val e = self.endPosition
    def loop(p: s.Position, r: T): T =
      if (p.eq(e)) {
        r
      } else {
        loop(self.positionAfter(p), combine(r, self.at(p)))
      }
    loop(self.startPosition, partialResult)

  /** Applies `action` on each element of `self`, in order, until `action` returns `false`, and
    * returns `false` iff `action` did.
    *
    * You can return `false` from `action` to emulate a `continue` statement as found in traditional
    * imperative languages (e.g., C).
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def forEach(action: (s.Element) => Boolean): Boolean =
    val e = self.endPosition
    def loop(p: s.Position): Boolean =
      if (p.eq(e)) {
        true
      } else if (!action(self.at(p))) {
        false
      } else {
        loop(self.positionAfter(p))
      }
    loop(self.startPosition)

  /** Returns the position of the first element of `self` satisfying `predicate`, or `None` if no
    * such element exists.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def firstPositionWhere(predicate: (s.Element) => Boolean): Option[s.Position] =
    val e = self.endPosition
    def loop(p: s.Position): Option[s.Position] =
      if (p.eq(e)) {
        None
      } else if (predicate(self.at(p))) {
        Some(p)
      } else {
        loop(self.positionAfter(p))
      }
    loop(self.startPosition)

  /** Returns the minimum element in `self`, using `isLessThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement(isLessThan: (s.Element, s.Element) => Boolean): Option[s.Element] =
    self.leastElement(isLessThan)

  // NOTE: I can't find a reasonable way to call this method.
  /** Returns the minimum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def minElement()(using Comparable[s.Element]): Option[s.Element] =
    self.minElement(isLessThan = _ lt _)

  /** Returns the maximum element in `self`, using `isGreaterThan` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement(isGreaterThan: (s.Element, s.Element) => Boolean): Option[s.Element] =
    self.leastElement(isGreaterThan)

  /** Returns the maximum element in `self`.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def maxElement()(using Comparable[s.Element]): Option[s.Element] =
    self.maxElement(isGreaterThan = _ gt _)

  /** Returns the maximum element in `self`, using `isOrderedBefore` to compare elements.
    *
    * @complexity
    *   O(n) where n is the number of elements in `self`.
    */
  def leastElement(isOrderedBefore: (s.Element, s.Element) => Boolean): Option[s.Element] =
    if (self.isEmpty) {
      None
    } else {
      val e = self.endPosition
      def _least(p: s.Position, least: s.Element): s.Element =
        if (p.eq(e)) {
          least
        } else {
          val x = self.at(p)
          _least(
            self.positionAfter(p),
            if (isOrderedBefore(x, least)) { x }
            else { least }
          )
        }

      val b = self.startPosition
      Some(_least(self.positionAfter(b), self.at(b)))
    }

}
