package hylo

/** A wrapper around an object providing a reference API. */
private final class Ref[T](val value: T) {

  override def toString: String =
    s"Ref($value)"

}

/** A type-erased rquatable value.
  *
  * An `AnyEquatable` forwards its operations to a wrapped value, hiding its implementation.
  */
final class AnyEquatable private (
    private val wrapped: AnyRef,
    private val _eq: (AnyRef, AnyRef) => Boolean
) {

  /** Returns `true` iff `this` and `other` have an equivalent value. */
  def eq(other: AnyEquatable): Boolean =
    _eq(this.wrapped, other.wrapped)

  /** Returns the value wrapped in `this` as an instance of `T`. */
  def unsafelyUnwrappedAs[T]: T =
    wrapped.asInstanceOf[Ref[T]].value

  /** Returns a textual description of `this`. */
  override def toString: String =
    wrapped.toString

}

object AnyEquatable {

  /** Creates an instance wrapping `wrapped`. */
  def apply[T](using Equatable[T])(wrapped: T): AnyEquatable =
    def eq(a: AnyRef, b: AnyRef) =
      a.asInstanceOf[Ref[T]].value eq b.asInstanceOf[Ref[T]].value
    new AnyEquatable(Ref(wrapped), eq)

}

given anyEquatableIsEquatable: Equatable[AnyEquatable] with {

  extension (self: AnyEquatable) def eq(other: AnyEquatable): Boolean = self.eq(other)

}
