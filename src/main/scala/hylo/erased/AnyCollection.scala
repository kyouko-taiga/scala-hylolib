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
    // NOTE: This evidence has to be redefined here otherwise the compiler gets confused when the
    // method is called on a collection of `Int`, reporting ambiguity between `intIsComparable`
    // and `anyEquatableIsEquatable`. None of these choices is correct, as the right evidence is
    // `b.positionIsEquatable`. Note also that the ambiguity is suppressed if the constructor of
    // `AnyEquatable` is declared with a context bound rather than an implicit parameter.
    given Equatable[b.Position] = b.positionIsEquatable

    def start(): AnyEquatable =
      AnyEquatable(base.startPosition)

    def end(): AnyEquatable =
      AnyEquatable(base.endPosition)

    def after(p: AnyEquatable): AnyEquatable =
      AnyEquatable(base.positionAfter(p.unsafelyUnwrappedAs[b.Position]))

    def at(p: AnyEquatable): b.Element =
      base.at(p.unsafelyUnwrappedAs[b.Position])

    new AnyCollection[b.Element](
      _start = start,
      _end = end,
      _after = after,
      _at = at
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
