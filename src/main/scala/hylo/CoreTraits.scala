package hylo

// ----------------------------------------------------------------------------
// Equatable
// ----------------------------------------------------------------------------

trait Equatable[Self] {

  /** Returns `true` iff `self` and `other` have an equivalent value. */
  extension (self: Self) def eq(other: Self): Boolean

}

extension [Self: Equatable](self: Self) def neq(other: Self): Boolean = !self.eq(other)

// ----------------------------------------------------------------------------
// Hashable
// ----------------------------------------------------------------------------

trait Hashable[Self] extends Equatable[Self] {

  /** Hashes the salient parts of `self` into `hasher`. */
  extension (self: Self) def hashInto(hasher: Hasher): Unit

}

// ----------------------------------------------------------------------------
// Comparable
// ----------------------------------------------------------------------------

trait Comparable[Self] extends Equatable[Self] {

  extension (self: Self) {

    /** Returns `true` iff `self` is ordered before `other`. */
    def lt(other: Self): Boolean

    /** Returns `true` iff `self` is ordered after `other`. */
    def gt(other: Self): Boolean = other.lt(self)

    /** Returns `true` iff `self` is equal to or ordered before `other`. */
    def le(other: Self): Boolean = !other.lt(self)

    /** Returns `true` iff `self` is equal to or ordered after `other`. */
    def ge(other: Self): Boolean = !self.lt(other)

  }

}

/** Returns the lesser of `x` and `y`. */
def min[T: Comparable](x: T, y: T): T =
  if y.lt(x) then y else x

/** Returns the greater of `x` and `y`. */
def max[T: Comparable](x: T, y: T): T =
  if x.lt(y) then y else x
