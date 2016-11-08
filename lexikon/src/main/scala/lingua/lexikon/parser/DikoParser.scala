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
package parser

import lingua.parser.Lexical

import untyped._

object DikoParser {

  val keywords =
    Set("alphabet", "as", "categories", "lexicon", "private", "rewrite", "separators", "tags")

  val lexical = new Lexical(keywords)

  import fastparse.noApi._
  import lexical._
  import WsApi._

  val category: P[Category] = P(
    (Index ~ name ~ keyword("as") ~ name ~ ";").map {
      case (idx, n, a) => Category(n, a)(idx)
    })

  val tag: P[Tag] = P(
    (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~ name ~ "{" ~/ (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~/ name ~ ";").map({
      case (idx, p, n, a) => Tag(p, n, a, Nil)(idx)
    }).rep(min = 1) ~ "}").map {
      case (idx, p, n, a, ts) => Tag(p, n, a, ts)(idx)
    }
      | (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~ name ~ ";").map {
        case (idx, p, n, a) => Tag(p, n, a, Nil)(idx)
      })

  def unit(name: String): P[DikoUnit] = P(
    section.rep(min = 0)).map(DikoUnit(name, _)) ~ End

  val section: P[Section] = P(
    alphabet
      | separators
      | tags
      | categories
      | lexicon
      | rewriteRule)

  val alphabet: P[Alphabet] = P(
    Index ~ keyword("alphabet") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 0) ~ ";").map {
      case (idx, chars) => Alphabet(chars)(idx)
    }

  val separators: P[Separators] = P(
    keyword("separators") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 1) ~ ";").map(Separators)

  val categories: P[Categories] = P(
    keyword("categories") ~/ category.rep(min = 1)).map(Categories)

  val tags: P[Tags] = P(
    keyword("tags") ~/ tag.rep(min = 1)).map(Tags)

  val lexicon: P[Lexicon] = P(
    (Index ~ keyword("lexicon") ~/ name ~ annotation ~ "{" ~/ word.rep(min = 0) ~/ "}").map {
      case (idx, name, (cat, tags), words) => Lexicon(name, cat, tags, words)(idx)
    })

  val word: P[Word] = P(
    (Index ~ wordChar.rep(min = 1) ~ ("/" ~/ annotation).?.map(_.getOrElse((None, Seq.empty[TagEmission]))) ~ ";").map {
      case (idx, chars, (cat, tags)) => Word(chars, cat, tags)(idx)
    })

  val optChar: P[Option[Char]] = P(
    char.!.map(c => Some(c(0)))
      | LiteralStr("_").map(_ => None))

  val wordChar: P[WordChar] = P(
    (optChar ~ ":" ~ optChar).map { case (c1, c2) => WordChar(c1, c2) }
      | char.!.map(c => WordChar(Some(c(0)), Some(c(0)))))

  val rewriteRule: P[RewriteRule] = P(
    (Index ~ keyword("rewrite") ~ name ~ annotation ~ ("=>" ~ tagEmission.rep(min = 1)).? ~ "{" ~/ `case`.rep(min = 1, sep = "|" ~/ Pass) ~ "}").map {
      case (idx, name, (cat, tagsin), Some(tagsout), cases) => RewriteRule(name, cat, tagsin, tagsout, cases)(idx)
      case (idx, name, (cat, tagsin), None, cases)          => RewriteRule(name, cat, tagsin, tagsin, cases)(idx)
    })

  val `case`: P[Case] = P(
    pattern ~ "=>" ~/ replacement).map { case (pat, repl) => Case(pat, repl) }

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
    ("@" ~ "(" ~ ".." ~ ")").map(_ => RecursiveReplacement)
      | P("..").map(_ => CaptureReplacement)
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
