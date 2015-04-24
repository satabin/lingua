package lingua
package lexikon

import org.parboiled2._

import scala.language.implicitConversions

import java.io.File

import scala.io.Source

import scala.annotation.tailrec

class DikoParser(val filename: String, val input: ParserInput) extends LinguaParser {

  import CharPredicate._

  def this(file: File) =
    this(file.getCanonicalPath, Source.fromFile(file).mkString)

  def dico = rule(
    keyword("alphabet") ~ zeroOrMore(capture(ANY) ~> (_(0)) ~ oneOrMore(WhiteSpace)) ~ ";"
      ~> ((alphabet: Seq[Char]) => keyword("categories") ~ zeroOrMore(category)
        ~ keyword("tags") ~ tags
        ~ keyword("main") ~ "=" ~ name ~ ";"
        ~ zeroOrMore(lexikon(alphabet))) ~ EOI)

  def lexikon(alphabet: Seq[Char]): Rule1[Lexikon] = rule(
    keyword("lexikon") ~ name ~ optional(default) ~ "{" ~ zeroOrMore((oneOrMore(transition(alphabet)) ~> Transitions | rewrite(alphabet)) ~ ";") ~ "}"
      ~> Lexikon)

  def transition(alphabet: Seq[Char]): Rule1[Transition] = rule(
    "_" ~ "/" ~ tagEmission ~> ((t: (Boolean, String)) => TagTransition(t._1, t._2))
      | capture(CharPredicate(alphabet)) ~> (_(0)) ~ "/" ~ capture(CharPredicate(alphabet)) ~> (_(0)) ~> InOutTransition
      | capture(oneOrMore(CharPredicate(alphabet))) ~> SimpleTransition)

  def rewrite(alphabet: Seq[Char]): Rule1[RewriteRule] = rule(
    keyword("rewrite") ~ name ~ defaultTags ~ "{" ~ oneOrMore(rewriteCase(alphabet) ~ ";") ~ "}" ~> RewriteRule)

  def rewriteCase(alphabet: Seq[Char]): Rule1[RewriteCase] = rule(
    keyword("case") ~ oneOrMore(transition(alphabet)) ~ "<=>" ~ oneOrMore(transition(alphabet)) ~> RewriteCase)

  def defaultTags: Rule1[Seq[(Boolean, String)]] = rule(
    "is" ~ oneOrMore(tagEmission)
      | push(Vector()))

  def default: Rule1[Default] = rule(
    "is" ~ optional(name) ~ zeroOrMore(tagEmission) ~> Default)

  def tagEmission: Rule1[(Boolean, String)] = rule(
    ("+" ~ push(true) | "-" ~ push(false)) ~ name ~> ((pres: Boolean, name: String) => (pres, name)))

}
