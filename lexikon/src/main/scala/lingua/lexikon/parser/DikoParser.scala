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
package lingua.lexikon
package parser

import lingua.parser._

object DikoParser {

  val keywords =
    Set("alphabet", "as", "categories", "lexikon", "main", "private", "rewrite", "rule", "separators", "tags")

  val utils = new LinguaParser(keywords)

  import fastparse.noApi._
  import utils._
  import lexical._
  import WsApi._

  val diko: P[Diko] = P(
    (keyword("alphabet") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 0) ~ ";"
      ~ (keyword("separators") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 1) ~ ";").?.map(_.getOrElse(Seq.empty[Char]))
      ~ (keyword("categories") ~/ category.rep(min = 1)).?.map(_.getOrElse(Seq.empty[Category]))
      ~ (keyword("tags") ~/ tag.rep(min = 1)).?.map(_.getOrElse(Seq.empty[Tag]))
      ~ NoCut(lexikon.rep(min = 0)) ~/ End).map {
        case (alphabet, separators, cats, tags, lexika) =>
          Diko(alphabet, separators, cats, tags, lexika)
      })

  val lexikon: P[Lexikon] = P(
    (Index ~ keyword("lexikon") ~/ name ~ annotation ~ "{" ~/ (rewrite | word).rep(min = 0) ~/ "}").map {
      case (idx, name, (cat, tags), entries) => Lexikon(name, cat, tags, entries)(idx)
    })

  val word: P[Word] = P(
    (Index ~ wordChar.rep(min = 1) ~ ("/" ~ annotation).?.map(_.getOrElse((None, Seq.empty[TagEmission]))) ~ ";").map {
      case (idx, chars, (cat, tags)) => Word(chars, cat, tags)(idx)
    })

  val optChar: P[Option[Char]] = P(
    char.!.map(c => Some(c(0)))
      | fastparse.parsers.Terminals.CharLiteral('_').map(_ => None))

  val wordChar: P[WordChar] = P(
    (optChar ~ ":" ~ optChar).map { case (c1, c2) => WordChar(c1, c2) }
      | char.!.map(c => WordChar(Some(c(0)), Some(c(0)))))

  val rewrite: P[Rewrite] = P(
    (Index ~ keyword("rewrite") ~ name ~ tagEmission.rep(min = 0) ~ "{" ~/ rule.rep(min = 1) ~ "}").map {
      case (idx, name, tags, rules) => Rewrite(name, tags, rules)(idx)
    })

  val rule: P[Rule] = P(
    keyword("rule") ~/ (pattern ~ "=>" ~/ replacement).rep(min = 1, sep = "|" ~/ Pass) ~ ";")

  val pattern: P[Pattern] = P(
    (Index
      ~ (P("..").map(_ => CapturePattern)
        | (!"=>" ~ char).rep(min = 1).!.map(StringPattern)).rep(min = 1)
        ~ ("/" ~ annotation).?.map(_.getOrElse((None, Seq.empty[TagEmission])))).map {
          case (idx, seq, (cat, tags)) =>
            Pattern(seq, cat, tags)(idx)
        })

  val replacement: P[Replacement] = P(
    (Index
      ~ replacementText.rep(min = 0)
      ~ ("/" ~ tagEmission.rep(min = 1)).?.map(_.getOrElse(Seq.empty[TagEmission]))).map {
        case (idx, seq, tags) =>
          Replacement(seq, tags)(idx)
      })

  val replacementText: P[CaseReplacement] = P(
    "@" ~ "(" ~ (replacementText.rep(min = 1) ~ ")").map(RecursiveReplacement)
      | "\\" ~ integer.map(i => CaptureReplacement(Some(i)))
      | P("..").map(_ => CaptureReplacement(None))
      | char.rep(min = 1).!.map(StringReplacement))

  val annotation: P[(Option[String], Seq[TagEmission])] = P(
    ("@" ~ name).? ~ tagEmission.rep(min = 0))

  val tagEmission: P[TagEmission] =
    (("+" | "-").! ~ name).map {
      case (pres, name) => (pres == "+", name)
    }.opaque("<tag emission>")

  val char: P[Unit] =
    (!CharIn("@(){};:./\\| _") ~ AnyChar).opaque("<character>")

  val integer: P[Int] =
    CharIn("0123456789").rep(min = 1).!.map(_.toInt).opaque("<integer>")

}
