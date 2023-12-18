package hylo

given intIsValue: Value[Int] with {

  extension (self: Int) {

    def copy(): Int =
      // Note: Scala's `Int` has value semantics already.
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

  }

}

given intIsComparable: Comparable[Int] with {

  extension (self: Int) {

    def copy(): Int =
      self

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Hasher =
      hasher.combine(self)

    def lt(other: Int): Boolean = self < other

  }

}

given intIsStringConvertible: StringConvertible[Int] with {}
