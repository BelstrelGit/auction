package ui

abstract class Screen protected(
  title: String,
  data: Element,
  bot: Element
) extends Element {

  import Element._

  val content = {
    val e = sepVBox(data, bot)
    val header = Header(title, e.width)

    vBox(header, e)
  }

  val width: Int = content.head.length
  val height: Int = content.length
}
