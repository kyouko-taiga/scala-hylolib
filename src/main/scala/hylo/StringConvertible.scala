package hylo

trait StringConvertible[Self] {

  extension (self: Self) {

    /** Returns a textual description of `self`. */
    def description: String =
      self.toString

  }

}
