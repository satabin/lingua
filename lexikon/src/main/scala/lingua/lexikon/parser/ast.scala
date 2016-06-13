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

case class Diko(alphabet: Seq[Char], separators: Seq[Char], categories: Seq[Category], tags: Seq[Tag], lexika: Seq[Lexikon])

case class Lexikon(name: String, category: Option[String], tags: Seq[TagEmission], entries: Seq[Entry])(val offset: Int)

sealed trait Entry

final case class WordChar(in: Option[Char], out: Option[Char])

final case class Word(input: Seq[WordChar], category: Option[String], tags: Seq[TagEmission])(val offset: Int) extends Entry {

  val word = input.collect { case WordChar(Some(c), _) => c }.mkString

}

final case class Rewrite(name: String, tags: Seq[TagEmission], rules: Seq[Rule])(val offset: Int) extends Entry

final case class Pattern(seq: Seq[CasePattern], category: Option[String], tags: Seq[TagEmission])(val offset: Int)

sealed trait CasePattern
final case class StringPattern(s: String) extends CasePattern
case object CapturePattern extends CasePattern

final case class Replacement(seq: Seq[CaseReplacement], tags: Seq[TagEmission])(val offset: Int)

sealed trait CaseReplacement
final case class StringReplacement(s: String) extends CaseReplacement
final case class CaptureReplacement(n: Option[Int]) extends CaseReplacement
final case class RecursiveReplacement(seq: Seq[CaseReplacement]) extends CaseReplacement
