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

/** Collection of parsers used to parse `.dico` files.
 *  A file is a [[untyped.DikoUnit unit]] and is composed of a sequence of [[untyped.Section sections]].
 *
 *
 *  @author Lucas Satabin
 */
object DikoParser {

  /** The set of keywords that are part of the description language.
   *  Keywords are shaped like identifiers but cannot used as such if they are not escaped
   *  between backticks (e.g. `alphabet`).
   */
  val keywords =
    Set("alphabet", "as", "categories", "lexicon", "morphology", "private", "rewrite", "separators", "tags")

  val lexical = new Lexical(keywords)

  import fastparse.noApi._
  import lexical._
  import WsApi._

  /** {{{
   *  Category ::= Name `as' Name `;'
   *  }}}
   */
  val category: P[Category] = P(
    (Index ~ name ~ keyword("as") ~ name ~ ";").map {
      case (idx, n, a) => Category(n, a)(idx)
    })

  /** {{{
   *  Tag ::= SimpleTag
   *        | Name `as` Name `{' SimpleTag* ' }'
   *
   *  SimpleTag ::= `private'? Name `as' Name `;'
   *  }}}
   */
  val tag: P[Tag] = P(
    (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~ name ~ "{" ~/ (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~/ name ~ ";").map({
      case (idx, p, n, a) => Tag(p, n, a, Nil)(idx)
    }).rep(min = 1) ~ "}").map {
      case (idx, p, n, a, ts) => Tag(p, n, a, ts)(idx)
    }
      | (Index ~ keyword("private").!.?.map(_.isEmpty) ~ name ~ keyword("as") ~ name ~ ";").map {
        case (idx, p, n, a) => Tag(p, n, a, Nil)(idx)
      })

  /** {{{
   *  DikoUnit ::= Section*
   *  }}}
   */
  def unit(name: String): P[DikoUnit] = P(
    section.rep(min = 0)).map(DikoUnit(name, _)) ~ End.opaque("EOF")

  /** {{{
   *  Section ::= Alphabet
   *            | Separators
   *            | Tags
   *            | Categories
   *            | Lexicon
   *            | RewriteRule
   *  }}}
   */
  val section: P[Section] = P(
    alphabet
      | separators
      | tags
      | categories
      | lexicon
      | rewriteRule)

  /** {{{
   *  Alphabet ::= `alphabet' Character* `;'
   *  }}}
   */
  val alphabet: P[Alphabet] = P(
    Index ~ keyword("alphabet") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 0) ~ ";").map {
      case (idx, chars) => Alphabet(chars)(idx)
    }

  /** {{{
   *  Separators ::= `separators' Character* `;'
   *  }}}
   */
  val separators: P[Separators] = P(
    keyword("separators") ~/ (!";" ~ AnyChar.!.map(_(0))).rep(min = 1) ~ ";").map(Separators)

  /** {{{
   *  Categories ::= `categories' Category+
   *  }}}
   */
  val categories: P[Categories] = P(
    keyword("categories") ~/ category.rep(min = 1)).map(Categories)

  /** {{{
   *  Tags ::= `tags' Tag+
   *  }}}
   */
  val tags: P[Tags] = P(
    keyword("tags") ~/ tag.rep(min = 1)).map(Tags)

  /** {{{
   *  Morphology ::= `morphology' MorphologyRule+
   *
   *  MorphologyRule ::= Char+
   *                   | Char+ `[' Char+ `]'
   *                   | Char+ `[' `^' Char+ `]'
   *  }}}
   */
  //val morphology: P[Morphology] = ???

  /** {{{
   *  Lexicon ::= `lexicon' Name (`@' Name)? (`+' Name | `-` Name)* `{' Word* `}'
   *  }}}
   */
  val lexicon: P[Lexicon] = P(
    (Index ~ keyword("lexicon") ~/ name ~ annotation ~ "{" ~/ word.rep(min = 0) ~/ "}").map {
      case (idx, name, (cat, tags), words) => Lexicon(name, cat, tags, words)(idx)
    })

  /** {{{
   *  Word ::= WordChar+ (`/' (`@' Name)? (`+' Name | `-` Name)*)?
   *
   *  WordChar ::= Char
   *             | Char `:' Char
   *             | `_' : Char
   *             | Char `:' `_'
   *             | `_' `:' `_'
   *  }}}
   */
  val word: P[Word] = P(
    (Index ~ wordChar.rep(min = 1) ~ ("/" ~/ annotation).?.map(_.getOrElse((None, Seq.empty[TagEmission]))) ~ ";").map {
      case (idx, chars, (cat, tags)) => Word(chars, cat, tags)(idx)
    })

  private val optChar: P[Option[Char]] = P(
    char.!.map(c => Some(c(0)))
      | LiteralStr("_").map(_ => None))

  private val wordChar: P[WordChar] = P(
    (optChar ~ ":" ~ optChar).map { case (c1, c2) => WordChar(c1, c2) }
      | char.!.map(c => WordChar(Some(c(0)), Some(c(0)))))

  /** {{{
   *  RewriteRule ::= `rewrite' Name (`@' Name)? (`+' Name | `-` Name)* (`=>' (`+' Name | `-` Name)*)? `{' Case (`|' Case)* `}'
   *
   *  Case ::= Pattern `=>' Replacement
   *
   *  Pattern ::= LexicalPattern+ (`/' (`@' Name)? (`+' Name | `-` Name)*)?
   *
   *  LexicalPattern ::= `..'
   *                   | Char
   *
   *  Replacement ::= LexicalReplacement* (`/' (`@' Name)? (`+' Name | `-` Name)*)?
   *
   *  LexicalReplacement ::= `..'
   *                       | Char
   *                       | `@' `(' `..' `)'
   *  }}}
   */
  val rewriteRule: P[RewriteRule] = P(
    (Index ~ keyword("rewrite") ~ name ~ annotation ~ ("=>" ~ tagEmission.rep(min = 1)).? ~ "{" ~/ `case`.rep(min = 1, sep = "|" ~/ Pass) ~ "}").map {
      case (idx, name, (cat, tagsin), Some(tagsout), cases) => RewriteRule(name, cat, tagsin, tagsout, cases)(idx)
      case (idx, name, (cat, tagsin), None, cases)          => RewriteRule(name, cat, tagsin, tagsin, cases)(idx)
    })

  private val `case`: P[Case] = P(
    pattern ~ "=>" ~/ replacement).map { case (pat, repl) => Case(pat, repl) }

  private val pattern: P[Pattern] = P(
    (Index
      ~ (P("..").map(_ => CapturePattern)
        | (!"=>" ~ char).rep(min = 1).!.map(StringPattern)).rep(min = 1)
        ~ ("/" ~ annotation).?.map(_.getOrElse((None, Seq.empty[TagEmission])))).map {
          case (idx, seq, (cat, tags)) =>
            Pattern(seq, cat, tags)(idx)
        })

  private val replacement: P[Replacement] = P(
    (Index
      ~ replacementText.rep(min = 0)
      ~ ("/" ~ tagEmission.rep(min = 0)).?.map(_.getOrElse(Seq.empty[TagEmission]))).map {
        case (idx, seq, tags) =>
          Replacement(seq, tags)(idx)
      })

  private val replacementText: P[CaseReplacement] = P(
    ("@" ~ "(" ~ ".." ~ ")").map(_ => RecursiveReplacement)
      | P("..").map(_ => CaptureReplacement)
      | char.rep(min = 1).!.map(StringReplacement))

  private val annotation: P[(Option[String], Seq[TagEmission])] = P(
    ("@" ~ name).? ~ tagEmission.rep(min = 0))

  private val tagEmission: P[TagEmission] =
    (("+" | "-").! ~ name).map {
      case (pres, name) => (pres == "+", name)
    }.opaque("<tag emission>")

  val char: P[Unit] =
    (!CharIn("@(){};:./\\| _") ~ AnyChar).opaque("<character>")

  val integer: P[Int] =
    CharIn("0123456789").rep(min = 1).!.map(_.toInt).opaque("<integer>")

}
