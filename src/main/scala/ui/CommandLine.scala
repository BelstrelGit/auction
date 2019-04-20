package ui

class CommandLine private(val content: Content) extends Element {
  val width: Int = content.head.length
  val height: Int = 1

  override def toString: String = content.mkString
}

object CommandLine {
  def apply(prompt: String): CommandLine = new CommandLine(s"$prompt")
}
