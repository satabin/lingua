package lingua

import org.parboiled2.{
  Parser,
  ParserInput,
  CharPredicate,
  Rule0,
  Rule1
}

import scala.language.implicitConversions

import scala.annotation.tailrec

import java.io.File

import scala.io.Source

trait LinguaParser extends Parser {

  val filename: String
  val input: ParserInput

  import CharPredicate.from

  protected[this] var lines = List(0)

  protected[this] def pos(startIdx: Int, endIdx: Int): Position = {
    @tailrec
    def loop(lines: List[Int]): Position = lines match {
      case start :: rest if startIdx < start =>
        // the index is located before last line, find it
        loop(rest)
      case start :: _ if startIdx >= start =>
        // the index is located in the current line
        Position(filename, lines.size, startIdx - start, startIdx, endIdx)
      case _ =>
        sys.error("This is a bug")
    }
    loop(lines)
  }

  implicit def wspStr(s: String): Rule0 =
    rule(str(s) ~ zeroOrMore(WhiteSpace))

  def keyword(kwd: String): Rule0 = rule(
    atomic(kwd) | fail(s"keyword `$kwd'"))

  def category: Rule1[Category] = rule(
    push(cursor) ~ name ~ keyword("as") ~ name ~> ((idx: Int, n: String, a: String) => Category(n, a)(pos(idx, cursor))))

  def tags: Rule1[Seq[Tag]] = rule(
    zeroOrMore(group | tag(None) ~> ((t: Tag) => Vector(t))) ~> ((tss: Seq[Seq[Tag]]) => tss.flatten))

  def group: Rule1[Seq[Tag]] = rule(
    name ~ "{" ~> ((n: String) => oneOrMore(tag(Some(n))) ~ "}"))

  def tag(g: Option[String]): Rule1[Tag] = rule(
    push(cursor) ~ name ~ keyword("as") ~ name ~> ((idx: Int, n: String, a: String) => Tag(n, a, g)(pos(idx, cursor))))

  def name: Rule1[String] = rule(
    capture(atomic(from(_.isLetterOrDigit) ~ zeroOrMore(optional(ch('-')) ~ from(_.isLetterOrDigit)) ~ zeroOrMore(str("'")))) ~ zeroOrMore(WhiteSpace))

  def alpha: Rule1[String] =
    rule(ch(''') ~ name)

  def string: Rule1[String] = rule(
    ch('"') ~ capture(zeroOrMore(noneOf("\"\r\n") ~ (ANY | escaped))) ~ ch('"'))

  def escaped: Rule0 = rule(
    str("\\\\") | str("\\\""))

  def WhiteSpace: Rule0 = rule(
    quiet(str("//") ~ zeroOrMore(!(EOL) ~ ANY)
      | anyOf(" \t\f")
      | EOL ~ run(if (cursor > lines.head) lines ::= cursor)))

  def EOL: Rule0 = rule(
    str("\n")
      | str("\r\n"))

}
