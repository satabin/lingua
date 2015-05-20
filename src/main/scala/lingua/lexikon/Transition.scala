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

sealed trait Entry

final case class Transitions(transitions: Seq[Transition]) extends Entry

sealed trait Transition

final case class GroupTransition(transitions: Seq[Transition]) extends Transition

final case class CaptureTransition(n: Int) extends Transition

final case class SimpleTransition(word: String) extends Transition

final case class InOutTransition(in: Option[Char], out: Option[Char], category: Option[String], tags: Seq[(Boolean, String)]) extends Transition

final case class TagTransition(category: Option[String], tags: Seq[(Boolean, String)]) extends Transition

final case class LexikonTransition(name: String) extends Transition

final case class RewriteRules(name: String, default: Default, cases: Seq[Seq[RewriteCase]]) extends Entry

final case class RewriteCase(inAffix: Option[Affix], trIn: Seq[Transition], outAffix: Option[Affix], trOut: Seq[Transition])

sealed trait Affix
case object Prefix extends Affix
case object Suffix extends Affix
