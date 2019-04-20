package ui

object Text {
  import Element._
  def apply(data: Any*): Element =
    data.foldLeft(empty)((acc, d) => vBox(acc, line(d.toString)))
}
