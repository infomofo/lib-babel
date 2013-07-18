package kom.infomofo.babel.util

import org.jsoup.Jsoup

/**
 * formats strings to konform to the following konstraints
 *  - kharakter set limited to the following:
 *    [abdefghijklmnoprstuvwz ,.]
 *  - string limited to a length of
 */
object Formatter {

  /*
   * "Each book contains 410 pages; each page, 40 lines; each line, about 80 black letters."
   * That makes 410 x 40 x 80 = 1,312,000 characters
   * */
  val PAGES_PER_BOOK: Int = 410
  val LINES_PER_PAGE: Int = 40
  val LETTERS_PER_LINE: Int = 80
  val LETTERS_PER_BOOK = PAGES_PER_BOOK * LINES_PER_PAGE * LETTERS_PER_LINE

  def SUBSTITUTION_MAP(x: Char): Array[Char] = {
    x match {
      case '1' => "one".toCharArray // This should be replaced with real number lokalization at some point i.e. ICU4J
      case '2' => "two".toCharArray
      case '3' => "three".toCharArray
      case '4' => "four".toCharArray
      case '5' => "five".toCharArray
      case '6' => "siks".toCharArray
      case '7' => "seven".toCharArray
      case '8' => "eight".toCharArray
      case '9' => "nine".toCharArray
      case 'q' => "k".toCharArray
      case 'c' => "k".toCharArray
      case 'x' => "ks".toCharArray
      case 'y' => "i".toCharArray
      case _ => Array(x)
    }
  }

  val LETTERS: Array[Char] = "abdefghijklmnoprstuvwz ,.".toCharArray

  def restrict(input: String): Array[Char] = {
    input
      .toLowerCase
      .toCharArray
      .flatMap ( (x:Char) =>
        SUBSTITUTION_MAP(x)
      )
      .filter(LETTERS.contains(_))
      .take(LETTERS_PER_BOOK)
  }

  def format(input: String): Iterator[Seq[Seq[String]]] = {
    restrict(input)
      .grouped(LETTERS_PER_LINE)
      .map(_.mkString)
      .grouped(LINES_PER_PAGE)
      .grouped(PAGES_PER_BOOK)
  }

  def formatPageContent (urlString: String): Iterator[Seq[Seq[String]]] = {
    val pageHtml: String = scala.io.Source.fromURL(urlString).getLines.mkString
    val doc = Jsoup.parse(pageHtml)
    val text = doc.body().text()
    format(text)
  }

  def print(book: Iterator[Seq[Seq[String]]]): Unit = {
    book
      .foreach(
        _.foreach(
          _.foreach(
            println _)
        )
    )
  }

  def isValidLetters(input: String): Boolean = (input == restrict(input))

  def main (args: Array[String]): Unit = {
    print(format("util-babel is a set of tools for formatting dokuments in a format suitable for inklusion in the librari of babel.\n\nit ensures that all entries into the librari are comprised of the twentytwo known letters in the alphabet.\n\nabcdefghijklmnoprstuvwz\n\nallowed punktuation is , ."))

    print(formatPageContent("https://en.wikipedia.org/wiki/The_Library_of_Babel"))

    print(formatPageContent("http://jubal.westnet.com/hyperdiscordia/library_of_babel.html"))

  }

}
