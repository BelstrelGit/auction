package ui

import org.scalacheck.Gen.{alphaNumChar, choose, identifier}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{Matchers, WordSpec}


class ElementSuite extends WordSpec with Matchers with PropertyChecks {

  import Element._
  import ElementSuite._

  "Element" when {

    "widen" should {
      "has width same as parameter if parameter > initial width and source.width if parameter less" in
        forAll(arbitraryElement.arbitrary, from1to100) { (el: Element, newWidth: Int) =>
          val expected = if (el.width > newWidth) el.width else newWidth
          el.widen(newWidth).width shouldEqual expected
        }

      "empty element return empty element" in forAll(from1to100) { newWidth: Int =>
        Element.empty.widen(newWidth) shouldEqual Element.empty
      }
    }

    "heighten" should {
      "has height same as parameter if parameter > initial height and source.height if parameter less" in
        forAll(arbitraryElement.arbitrary, from1to100) { (el: Element, newHeight: Int) =>
          val expected = if (el.height > newHeight) el.height else newHeight
          el.heighten(newHeight).height shouldEqual expected
        }

      "empty element return empty element" in forAll(from1to100) { newHeight: Int =>
        Element.empty.heighten(newHeight) shouldEqual Element.empty
      }
    }

    "hPosition" should {
      "if Start position then collect all content at left" in forAll(tests) {
        (_, el2, _, _, _, _, res, _, _, _, _, _) =>
          el2.hPosition(Start).toString shouldEqual res
      }

      "if Center position then collect all content at center" in forAll(tests) {
        (_, el2, _, _, _, _, _, res, _, _, _, _) =>
          el2.hPosition(Center).toString shouldEqual res
      }

      "if End position then collect all content at right" in forAll(tests) {
        (_, el2, _, _, _, _, _, _, res, _, _, _) =>
          el2.hPosition(End).toString shouldEqual res
      }

      "on Element.empty return Element.empty" in forAll { position: Positions =>
        Element.empty.hPosition(position) shouldEqual Element.empty
      }
    }

    "vPosition" should {
      "if Start position then collect all content at top" in forAll(tests) {
        (el1, _, _, _, _, _, _, _, _, res, _, _) =>
          el1.vPosition(Start).toString shouldEqual res
      }

      "if Center position then collect all content at center" in forAll(tests) {
        (el1, _, _, _, _, _, _, _, _, _, res, _) =>
          el1.vPosition(Center).toString shouldEqual res
      }

      "if End position then collect all content at bottom" in forAll(tests) {
        (el1, _, _, _, _, _, _, _, _, _, _, res) =>
          el1.vPosition(End).toString shouldEqual res
      }

      "on Element.empty return Element.empty" in forAll { position: Positions =>
        Element.empty.vPosition(position) shouldEqual Element.empty
      }
    }

    "new element from line(String)" should {
      "has content eq source string" in forAll { source: String =>
        line(source).toString shouldEqual source
      }

      "has width eq source length" in forAll { source: String =>
        line(source).width shouldEqual source.length
      }

      "has height eq 1" in forAll { source: String =>
        line(source).height shouldEqual 1
      }
    }

    "new element from hBox(Element*)" should {
      "has content with all source elements stacked at right" in forAll(tests) {
        (el1, el2, res, _, _, _, _, _, _, _, _, _) =>
          hBox(el1, el2).toString shouldEqual res
      }

      "has width eq sum source widths" in forAll { (el1: Element, el2: Element, el3: Element) =>
        hBox(el1, el2, el3).width shouldEqual (el1.width + el2.width + el3.width)
      }

      "has height eq max of source heights" in forAll { (el1: Element, el2: Element, el3: Element) =>
        hBox(el1, el2, el3).height shouldEqual max(el1.height, el2.height, el3.height)
      }
    }

    "new element from sepHBox(Element*)" should {
      "has content with all source elements stacked at right and separated by vSep" in
        forAll(tests) { (el1, el2, _, res, _, _, _, _, _, _, _, _) =>
          sepHBox(el1, el2).toString shouldEqual res
        }

      "has width eq sum source widths and plus 1 for every sep (between elements)" in
        forAll { (el1: Element, el2: Element, el3: Element) =>
          sepHBox(el1, el2, el3).width shouldEqual (el1.width + 1 + el2.width + 1 + el3.width)
        }

      "has height eq max of source heights" in forAll { (el1: Element, el2: Element, el3: Element) =>
        sepHBox(el1, el2, el3).height shouldEqual max(el1.height, el2.height, el3.height)
      }
    }

    "new element from vBox(Element*)" should {
      "has content with all source elements stacked at bottom" in forAll(tests) {
        (el1, el2, _, _, res, _, _, _, _, _, _, _) =>
          vBox(el1, el2).toString shouldEqual res
      }

      "has width eq max of source widths" in forAll { (el1: Element, el2: Element, el3: Element) =>
        vBox(el1, el2, el3).width shouldEqual max(el1.width, el2.width, el3.width)
      }

      "has height eq sum source heights" in forAll { (el1: Element, el2: Element, el3: Element) =>
        vBox(el1, el2, el3).height shouldEqual (el1.height + el2.height + el3.height)
      }
    }

    "new element from sepVBox(Element*)" should {
      "has content with all source elements stacked at bottom and separated by vSep" in
        forAll(tests) { (el1, el2, _, _, _, res, _, _, _, _, _, _) =>
          sepVBox(el1, el2).toString shouldEqual res
        }

      "has width eq max of source widths" in
        forAll { (el1: Element, el2: Element, el3: Element) =>
          sepVBox(el1, el2, el3).width shouldEqual max(el1.width, el2.width, el3.width)
        }

      "has height eq sum source heights and plus 1 for every sep (between elements)" in
        forAll { (el1: Element, el2: Element, el3: Element) =>
          sepVBox(el1, el2, el3).height shouldEqual (el1.height + 1 + el2.height + 1 + el3.height)
        }
    }

    "new element from uniform(Char, Int, Int)" should {
      "has content filled by same character" in forAll(alphaNumChar, from1to100, from1to100) {
        (s: Char, w: Int, h: Int) =>
          uniform(s, w, h).toString shouldEqual Vector.fill(h)(s.toString * w).mkString("\n")
      }

      "has width eq width param" in forAll(alphaNumChar, from1to100, from1to100) {
        (s: Char, width: Int, h: Int) =>
          uniform(s, width, h).width shouldEqual width
      }

      "has height eq height param" in forAll(alphaNumChar, from1to100, from1to100) {
        (s: Char, w: Int, height: Int) =>
          uniform(s, w, height).height shouldEqual height
      }
    }
  }
}

object ElementSuite {

  val from1to100: Gen[Int] = choose(1, 100)

  implicit val arbitraryPositions: Arbitrary[Positions] = Arbitrary(Gen.oneOf(Start, Center, End))

  implicit def arbitraryElement: Arbitrary[Element] =
    Arbitrary(identifier.flatMap(s =>
      from1to100.map(n =>
        Element(Vector.fill(n)(s)))
    ))

  def max(n1: Int, n2: Int, n3: Int): Int = Math.max(Math.max(n1, n2), n3)

  final case class TestCase(
    el1: Element,
    el2: Element,
    hBoxString: String,
    hBoxSepString: String,
    vBoxString: String,
    vBoxSepString: String,
    hPositionStart: String,
    hPositionCenter: String,
    hPositionEnd: String,
    vPositionStart: String,
    vPositionCenter: String,
    vPositionEnd: String
  ) {
    def asTuple = (el1, el2,
      hBoxString, hBoxSepString,
      vBoxString, vBoxSepString,
      hPositionStart, hPositionCenter, hPositionEnd,
      vPositionStart, vPositionCenter, vPositionEnd)
  }

  val ls: String = System.lineSeparator()

  val testCases: Vector[TestCase] = Vector(
    TestCase(
      Element(Vector(
        "        ",
        "****    ",
        "       -",
        "        ",
        "***     ",
        "        ",
        "        ",
        "        ")),

      Element(Vector(
        "****               ",
        "       **          ",
        "***            ----")),

      new StringBuilder() //hBoxString
        .append("        ****               ").append(ls)
        .append("****           **          ").append(ls)
        .append("       -***            ----").append(ls)
        .append("                           ").append(ls)
        .append("***                        ").append(ls)
        .append("                           ").append(ls)
        .append("                           ").append(ls)
        .append("                           ").toString,

      new StringBuilder() //hBoxSepString
        .append("        |****               ").append(ls)
        .append("****    |       **          ").append(ls)
        .append("       -|***            ----").append(ls)
        .append("        |                   ").append(ls)
        .append("***     |                   ").append(ls)
        .append("        |                   ").append(ls)
        .append("        |                   ").append(ls)
        .append("        |                   ").toString,

      new StringBuilder() //vBoxString
        .append("                   ").append(ls)
        .append("****               ").append(ls)
        .append("       -           ").append(ls)
        .append("                   ").append(ls)
        .append("***                ").append(ls)
        .append("                   ").append(ls)
        .append("                   ").append(ls)
        .append("                   ").append(ls)
        .append("****               ").append(ls)
        .append("       **          ").append(ls)
        .append("***            ----").toString,

      new StringBuilder() //vBoxSepString
        .append("                   ").append(ls)
        .append("****               ").append(ls)
        .append("       -           ").append(ls)
        .append("                   ").append(ls)
        .append("***                ").append(ls)
        .append("                   ").append(ls)
        .append("                   ").append(ls)
        .append("                   ").append(ls)
        .append("-------------------").append(ls)
        .append("****               ").append(ls)
        .append("       **          ").append(ls)
        .append("***            ----").toString,

      new StringBuilder() //hPositionStart
        .append("****               ").append(ls)
        .append("**                 ").append(ls)
        .append("***            ----").toString,

      new StringBuilder() //hPositionCenter
        .append("       ****        ").append(ls)
        .append("        **         ").append(ls)
        .append("***            ----").toString,

      new StringBuilder() //hPositionEnd
        .append("               ****").append(ls)
        .append("                 **").append(ls)
        .append("***            ----").toString,

      new StringBuilder() //vPositionStart
        .append("****    ").append(ls)
        .append("       -").append(ls)
        .append("        ").append(ls)
        .append("***     ").append(ls)
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("        ").toString,

      new StringBuilder() //vPositionCenter
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("****    ").append(ls)
        .append("       -").append(ls)
        .append("        ").append(ls)
        .append("***     ").append(ls)
        .append("        ").append(ls)
        .append("        ").toString,

      new StringBuilder() //vPositionEnd
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("        ").append(ls)
        .append("****    ").append(ls)
        .append("       -").append(ls)
        .append("        ").append(ls)
        .append("***     ").toString
    )
  )

  val tests = Table(
    ("el1", "el2",
      "hBoxString", "hBoxSepString",
      "vBoxString", "vBoxSepString",
      "hPositionStart", "hPositionCenter", "hPositionEnd",
      "vPositionStart", "vPositionCenter", "vPositionEnd",
    ), testCases.map(_.asTuple): _*)
}
