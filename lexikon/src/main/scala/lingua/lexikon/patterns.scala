/* Copyright (c) 2016 Lucas Satabin
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

/** A rewrite rule pattern.
 *
 *  @group Pattern
 */
sealed trait CasePattern

/** A sequence of characters to be matched in order.
 *
 *  @group Pattern
 */
final case class StringPattern(s: String) extends CasePattern {
  override def toString = s
}

/** matches any (possible empty) sequence of characters and capture the matched
 *  sequence to be used in [[Replacement]].
 *
 *  @group Pattern
 */
case object CapturePattern extends CasePattern {
  override def toString = ".."
}
