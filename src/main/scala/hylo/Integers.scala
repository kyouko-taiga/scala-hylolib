package hylo

given intIsHashable: Hashable[Int] with {

  extension (self: Int) {

    def eq(other: Int): Boolean =
      self == other

    def hashInto(hasher: Hasher): Unit =
      hasher.combine(self)

  }

}

given intIsComparable: Comparable[Int] with {

  extension (self: Int) {

    def eq(other: Int): Boolean = self == other

    def lt(other: Int): Boolean = self < other

  }

}
