package object ui {

  type Content = Vector[String]

  val blank = " "
  val vSep = '|'
  val hSep = '-'

  val emptyContent: Vector[Nothing] = Vector.empty

  implicit def string2Content(str: String): Content = str.split("\n").toVector

  implicit def element2Content(e:Element):Content = e.content

  implicit class StringWrapper(val s: String) extends AnyVal {
    def noContent: Boolean = s.isEmpty || s.forall((c: Char) => c == ' ')
  }
}
