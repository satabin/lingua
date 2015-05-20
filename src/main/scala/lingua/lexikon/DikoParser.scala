/* Copyright (c) 2015 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
    keyword("lexikon") ~ name ~ default ~ "{" ~ zeroOrMore(rewrite(alphabet) | oneOrMore(transition(alphabet)) ~ ";" ~> Transitions) ~ "}" ~> Lexikon
      | keyword("lexikon") ~ name ~ default ~ ";" ~> ((name: String, dflt: Default) => Lexikon(name, dflt, Seq())))

  def transition(alphabet: Set[Char]): Rule1[Transition] = rule(
    "[" ~ oneOrMore(transition(alphabet)) ~ "]" ~> GroupTransition
      | "->" ~ name ~> LexikonTransition
      | ch('\\') ~ integer ~> CaptureTransition
      | "/" ~ optional("@" ~ name) ~ oneOrMore(tagEmission) ~> TagTransition
      | char(alphabet) ~ "/" ~ char(alphabet) ~ optional("@" ~ name) ~ zeroOrMore(tagEmission) ~> InOutTransition
      | capture(CharPredicate(alphabet) ~ oneOrMore(CharPredicate(alphabet)) ~ zeroOrMore(WhiteSpace)) ~> SimpleTransition
      | char(alphabet) ~> ((in: Option[Char]) => InOutTransition(in, None, None, Seq())))

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
      | capture(CharPredicate(alphabet)) ~ zeroOrMore(WhiteSpace) ~> ((s: String) => Some(s(0)))
      | ch('\\') ~ capture(anyOf(" @[]/")) ~ zeroOrMore(WhiteSpace) ~> ((s: String) => Some(s(0))))

  def integer: Rule1[Int] = rule(
    capture(oneOrMore(anyOf("0123456789"))) ~ zeroOrMore(WhiteSpace) ~> ((s: String) => s.toInt))

}
