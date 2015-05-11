package lingua
package lexikon

import org.parboiled2._

import java.io.File

import scala.io.Source

import scala.annotation.tailrec

import shapeless._

class DikoParser(val filename: String, val input: ParserInput) extends LinguaParser {

  import CharPredicate._

  def this(file: File) =
    this(file.getCanonicalPath, Source.fromFile(file).mkString)

  val keywords =
    Set("alphabet", "as", "categories", "lexikon", "main", "rewrite", "rule", "tags")

  def dico: Rule1[Diko] = rule(
    keyword("alphabet") ~ zeroOrMore(!";" ~ capture(ANY) ~> (_(0)) ~ zeroOrMore(WhiteSpace)) ~ ";"
      ~> ((alphabet: Seq[Char]) => push(alphabet)
        ~!~ keyword("categories") ~!~ zeroOrMore(category)
        ~!~ keyword("tags") ~ tags
        ~!~ keyword("main") ~ "=" ~ name ~ ";"
        ~!~ zeroOrMore(lexikon(alphabet.toSet))) ~ EOI ~> Diko)

  def lexikon(alphabet: Set[Char]): Rule1[Lexikon] = rule(
    keyword("lexikon") ~ name ~ default ~ "{" ~ zeroOrMore(oneOrMore(transition(alphabet)) ~ ";" ~> Transitions | rewrite(alphabet)) ~ "}" ~> Lexikon
      | keyword("lexikon") ~ name ~ default ~ ";" ~> ((name: String, dflt: Default) => Lexikon(name, dflt, Seq())))

  def transition(alphabet: Set[Char]): Rule1[Transition] = rule(
    "&" ~ name ~> LexikonTransition
      | tagEmission ~> ((t: (Boolean, String)) => TagTransition(t._1, t._2))
      | char(alphabet) ~ "/" ~ char(alphabet) ~ optional("@" ~ name) ~> InOutTransition
      | capture(CharPredicate(alphabet) ~ oneOrMore(CharPredicate(alphabet)) ~ zeroOrMore(WhiteSpace)) ~> SimpleTransition
      | char(alphabet) ~> ((in: Option[Char]) => InOutTransition(in, None, None)))

  def rewrite(alphabet: Set[Char]): Rule1[RewriteRules] = rule(
    keyword("rewrite") ~ name ~ default ~ "{" ~ oneOrMore(rewriteRule(alphabet)) ~ "}" ~> RewriteRules)

  def rewriteRule(alphabet: Set[Char]): Rule1[Seq[RewriteCase]] = rule(
    keyword("rule")
      ~ oneOrMore(affixTransitions(alphabet) ~ "=>" ~ affixTransitions(alphabet)
        ~> ((inAffix: Option[Affix], in: Seq[Transition], outAffix: Option[Affix], out: Seq[Transition]) => RewriteCase(inAffix, in, outAffix, out))).separatedBy("|") ~ ";")

  def affixTransitions(alphabet: Set[Char]): Rule2[Option[Affix], Seq[Transition]] = rule(
    ">" ~ push(Some(Suffix)) ~ oneOrMore(transition(alphabet))
      | push(Some(Prefix)) ~ oneOrMore(transition(alphabet)) ~ "<"
      | push(None) ~ oneOrMore(transition(alphabet)))

  def default: Rule1[Default] = rule(
    optional("@" ~ name) ~ zeroOrMore(tagEmission) ~> ((name: Option[String], tags: Seq[(Boolean, String)]) => Default(name, tags)))

  def tagEmission: Rule1[(Boolean, String)] = rule(
    ("+" ~ push(true) | "-" ~ push(false)) ~ name ~> ((pres: Boolean, name: String) => (pres, name)))

  def char(alphabet: Set[Char]): Rule1[Option[Char]] = rule(
    "_" ~ push(None)
      | capture(CharPredicate(alphabet)) ~ zeroOrMore(WhiteSpace) ~> ((s: String) => Some(s(0))))

}
