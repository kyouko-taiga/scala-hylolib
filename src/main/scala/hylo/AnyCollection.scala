package hylo

/** A type-erased collection.
  *
  * A `AnyCollection` forwards its operations to a wrapped value, hiding its implementation.
  */
final class AnyCollection[Element] private (
    val _start: () => AnyEquatable,
    val _end: () => AnyEquatable,
    val _after: (AnyEquatable) => AnyEquatable,
    val _at: (AnyEquatable) => Element
)

object AnyCollection {

  /** Creates an instance forwarding its operations to `base`. */
  def apply[Base](using b: Collection[Base])(base: Base): AnyCollection[b.Element] =
    new AnyCollection[b.Element](
      _start = () => AnyEquatable(base.startPosition),
      _end = () => AnyEquatable(base.endPosition),
      _after = (p) => AnyEquatable(base.positionAfter(p.unsafelyUnwrappedAs[b.Position])),
      _at = (p) => base.at(p.unsafelyUnwrappedAs[b.Position])
    )

}

given anyCollectionIsCollection[T]: Collection[AnyCollection[T]] with {

  type Element = T

  type Position = AnyEquatable
  given positionIsEquatable: Equatable[Position] = anyEquatableIsEquatable

  extension (self: AnyCollection[T]) {

    def startPosition =
      self._start()

    def endPosition =
      self._end()

    def positionAfter(p: Position) =
      self._after(p)

    def at(p: Position) =
      self._at(p)

  }

}
