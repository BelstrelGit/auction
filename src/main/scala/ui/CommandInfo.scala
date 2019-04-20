package ui

class CommandInfo private (val content: Content) extends Element {
  val width: Int = content.head.length
  val height: Int = content.length
}

object CommandInfo{
  import Element._

  def apply(commands: List[String]): CommandInfo =
    new CommandInfo(
    vBox(
      line("Commands:"),
      commands.foldLeft(empty)((acc, c) => vBox(acc, line(s"  -$c")))
    )
  )
}
