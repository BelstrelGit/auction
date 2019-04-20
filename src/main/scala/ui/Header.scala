package ui

class Header(title: String, val width: Int, boarder: Char = hSep) extends Element {

  import Element._

  val content: Content =
    vBox(
      uniform(boarder, width, 1),
      line(title).widen(width).hPosition(Center),
      uniform(boarder, width, 1)
    ).content

  val height: Int = content.length
}

object Header {
  def apply(title: String, width: Int, boarder: Char = hSep): Header = {

    val adaptWidth =
      if (width < title.length) title.length
      else width

    new Header(title, adaptWidth, boarder)
  }
}
