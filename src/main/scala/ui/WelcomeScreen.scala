package ui

class WelcomeScreen(data: Element) extends Screen(
  "Auction client",
  data,
  CommandLine("Enter command:"))

object WelcomeScreen {
  import Element._
  def apply(): WelcomeScreen =
    new WelcomeScreen(
      sepHBox(
        Text("Welcome to Auction app.","At the left commands that you can use at that screen.").expandTo(79,15),
        CommandInfo(List("sing in", "sing up")).expandTo(20, 15))
    )
}