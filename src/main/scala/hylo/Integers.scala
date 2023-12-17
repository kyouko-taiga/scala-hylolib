package hylo

given intIsComparable: Comparable[Int] with {

  extension (self: Int) {

    def eq(other: Int): Boolean = self == other

    def lt(other: Int): Boolean = self < other

  }

}
