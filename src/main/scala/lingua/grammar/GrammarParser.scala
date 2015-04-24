package lingua
package grammar

import org.parboiled2._

import scala.language.implicitConversions

import scala.annotation.tailrec

import java.io.File

import scala.io.Source

class GrammarParser(val filename: String, val input: ParserInput) extends LinguaParser {

  def this(file: File) =
    this(file.getCanonicalPath, Source.fromFile(file).mkString)

  def grammar: Rule1[Grammar] = rule(
    keyword("categories") ~ oneOrMore(category)
      ~!~ keyword("tags") ~!~ tags
      ~!~ (push(cursor) ~ keyword("sentence") ~ "=" ~ name ~ ";" ~> ((idx: Int, n: String) => (n, pos(idx, cursor))))
      ~!~ zeroOrMore(production) ~ EOI ~> Grammar)

  def production: Rule1[Production] = rule(
    push(cursor) ~ name ~ "=" ~ oneOrMore(rhs) ~ ";" ~> ((idx: Int, n: String, rhs: Seq[Rhs]) => Production(n, rhs)(pos(idx, cursor))))

  def rhs: Rule1[Rhs] = rule(
    "[" ~ oneOrMore(oneOrMore(rhs)).separatedBy("|") ~ "]" ~> Alternative
      | "(" ~ oneOrMore(rhs) ~ ")" ~> Optional
      | string ~> Literal
      | name ~ atomic("<-" ~ "{") ~!~ zeroOrMore(name).separatedBy(",") ~ "}" ~ "-" ~> Abstract
      | name ~ ("{" ~ zeroOrMore(("+" ~ push(true) | "-" ~ push(false)) ~ name ~> (_ -> _)).separatedBy(",") ~ "}" | push(Seq()))
      ~> ((name: String, tags: Seq[(Boolean, String)]) => Symbol(name, tags.toSet)))

}
