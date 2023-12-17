package hylo

/** A wrapper around an object providing a reference API. */
private final class Ref[T](val value: T) {

  override def toString: String =
    s"Ref($value)"

}

/** A type-erased equatable value.
  *
  * An `AnyEquatable` forwards its operations to a wrapped value, hiding its implementation.
  */
final class AnyEquatable private (
    private val wrapped: AnyRef,
    private val _copy: (AnyRef) => AnyEquatable,
    private val _eq: (AnyRef, AnyRef) => Boolean,
    private val _hashInto: (AnyRef, Hasher) => Unit
) {

  /** Returns a copy of `this`. */
  def copy(): AnyEquatable =
    _copy(this.wrapped)

  /** Returns `true` iff `this` and `other` have an equivalent value. */
  def eq(other: AnyEquatable): Boolean =
    _eq(this.wrapped, other.wrapped)

  /** Hashes the salient parts of `this` into `hasher`. */
  def hashInto(hasher: Hasher): Unit =
    _hashInto(this.wrapped, hasher)

  /** Returns the value wrapped in `this` as an instance of `T`. */
  def unsafelyUnwrappedAs[T]: T =
    wrapped.asInstanceOf[Ref[T]].value

  /** Returns a textual description of `this`. */
  override def toString: String =
    wrapped.toString

}

object AnyEquatable {

  /** Creates an instance wrapping `wrapped`. */
  def apply[T](using Value[T])(wrapped: T): AnyEquatable =
    def copy(a: AnyRef): AnyEquatable =
      AnyEquatable(a.asInstanceOf[Ref[T]].value.copy())

    def eq(a: AnyRef, b: AnyRef): Boolean =
      a.asInstanceOf[Ref[T]].value eq b.asInstanceOf[Ref[T]].value

    def hashInto(a: AnyRef, hasher: Hasher): Unit =
      a.asInstanceOf[Ref[T]].value.hashInto(hasher)

    new AnyEquatable(Ref(wrapped), copy, eq, hashInto)

}

given anyEquatableIsValue: Value[AnyEquatable] with {

  extension (self: AnyEquatable) {

    def copy(): AnyEquatable =
      self.copy()

    def eq(other: AnyEquatable): Boolean =
      self eq other

    def hashInto(hasher: Hasher): Unit =
      self.hashInto(hasher)

  }

}
