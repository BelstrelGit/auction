package ui

abstract class Element {

  val content: Content
  val width: Int
  val height: Int

  protected def above(that: Element): Element = {
    val this1 = this.widen(that)
    val that1 = that.widen(this)

    Element(this1.content ++ that1.content)
  }

  protected def beside(that: Element): Element = {
    val this1 = this.heighten(that)
    val that1 = that.heighten(this)

    Element(this1.content.zip(that1.content).map { case (r, r1) => r + r1 })
  }

  def widen(width: Int): Element = {
    val diff = width - this.width

    if (diff > 0) Element(this.content.map(str => str + blank * diff))
    else this
  }

  def widen(that: Element): Element = widen(that.width)

  def heighten(height: Int): Element = {
    val diff = height - this.height

    if (diff > 0) Element(this.content ++ Vector.fill(diff)(blank * this.width))
    else Element(this.content)
  }

  def heighten(that: Element): Element = heighten(that.height)

  def expandTo(width: Int, height: Int): Element = widen(width).heighten(height)

  def vPosition(pos: Positions): Element = pos match {
    case Start =>
      val (emp, rest) = content.span(_.noContent)
      Element(rest ++ emp)

    case Center =>
      val stEmpLen = content.takeWhile(_.noContent).length
      val endEmpLen = content.reverse.takeWhile(_.noContent).length
      val half = (stEmpLen + endEmpLen) / 2
      val (stIndent, endIndent) = (half, half + ((stEmpLen + endEmpLen) % 2))
      Element(
        Vector.fill(stIndent)(blank * width) ++
          content.slice(stEmpLen, content.length - endEmpLen) ++
          Vector.fill(endIndent)(blank * width)
      )

    case End =>
      val (emp, r) = content.reverse.span(_.noContent)
      Element((r ++ emp).reverse)
  }


  def hPosition(pos: Positions): Element = pos match {
    case Start =>
      Element(content.map(str => {
        val trimmed = str.trim
        s"$trimmed${blank * (str.length - trimmed.length)}"
      }))

    case Center =>
      Element(content.map(str => {
        val trimmed = str.trim
        val diff = str.length - trimmed.length
        val half = diff / 2
        s"${blank * half}$trimmed${blank * (half + (diff % 2))}"
      }))

    case End =>
      Element(content.map(str => {
        val trimmed = str.trim
        s"${blank * (str.length - trimmed.length)}$trimmed"
      }))
  }

  override def toString: String = content.mkString("\n")
}

object Element {

  def apply(c: Content): Element = c match {
    case Vector() => empty
    case _ => new Element {
      override val content: Content = c
      override val width: Int = c.head.length
      override val height: Int = c.length
    }
  }

  def line(content: String): Element = apply(content.replace('\n', ' '))

  def hBox(elems: Element*): Element =
    elems.foldLeft(empty)((acc, el) => acc beside el)

  def sepHBox(elems: Element*): Element = elems.toList match {
    case Nil => empty
    case _ => elems.reduce((n1, n2) => n1 beside uniform(vSep, 1, Math.max(n1.height, n2.height)) beside n2)
  }

  def vBox(elems: Element*): Element =
    elems.foldLeft(empty)((acc, el) => acc above el)

  def sepVBox(elems: Element*): Element = elems.toList match {
    case Nil => empty
    case _ => elems.reduce((n1, n2) => n1 above uniform(hSep, Math.max(n1.width, n2.width), 1) above n2)
  }

  def uniform(symbol: Char, width: Int, height: Int): Element =
    if (width == 0 || height == 0) empty
    else Element(Vector.fill(height)(symbol.toString * width))

  def empty: Element = Empty

  private object Empty extends Element {

    val content: Content = emptyContent
    val width: Int = 0
    val height: Int = 0

    override def above(that: Element): Element = Element(that.content)

    override def beside(that: Element): Element = Element(that.content)

    override def widen(width: Int): Element = this

    override def heighten(width: Int): Element = this

    override def hPosition(pos: Positions): Element = this

    override def vPosition(pos: Positions): Element = this
  }

}


