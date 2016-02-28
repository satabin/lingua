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
package parser

case class Tag(fullname: String, alias: String, children: Seq[Tag])(val offset: Int)

case class Category(fullname: String, alias: String)(val offset: Int)

case class Diko(alphabet: Seq[Char], separators: Seq[Char], categories: Seq[Category], tags: Seq[Tag], lexika: Seq[Lexikon])

case class Lexikon(name: String, category: Option[String], tags: Seq[TagEmission], entries: Seq[Entry])(val offset: Int)

sealed trait Entry

final case class Word(word: Vector[Char], category: Option[String], tags: Seq[TagEmission])(val offset: Int) extends Entry

final case class Rewrite(name: String, tags: Seq[TagEmission], rules: Seq[Rule])(val offset: Int) extends Entry

final case class Pattern(affix: Affix, seq: Seq[CasePattern], category: Option[String], tags: Seq[TagEmission])(val offset: Int)

sealed trait CasePattern
final case class CharPattern(c: Char) extends CasePattern
final case class CapturePattern(n: Int) extends CasePattern
case object EmptyPattern extends CasePattern

sealed trait Affix
case object Prefix extends Affix
case object Suffix extends Affix
case object Infix extends Affix
case object NoAffix extends Affix

final case class Replacement(affix: Affix, seq: Seq[CaseReplacement], tags: Seq[TagEmission])(val offset: Int)

sealed trait CaseReplacement
final case class CharReplacement(c: Char) extends CaseReplacement
final case class CaptureReplacement(n: Int) extends CaseReplacement
final case class GroupReplacement(seq: Seq[CaseReplacement]) extends CaseReplacement
final case class NextReplacement(name: String) extends CaseReplacement
case object DropReplacement extends CaseReplacement
