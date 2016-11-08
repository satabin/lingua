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

/** A rewrite rule replacement text.
 *
 *  @group Replacement
 */
sealed trait CaseReplacement

/** A sequence of characters to replace with in order.
 *
 *  @group Replacement
 */
final case class StringReplacement(s: String) extends CaseReplacement {
  override def toString = s
}

/** A captured sequence of characters to replace with in order.
 *  The first captured group in the [[Pattern]] is placed at the first occurrence of
 *  a captured replacement.
 *
 *  @group Replacement
 */
case object CaptureReplacement extends CaseReplacement {
  override def toString = ".."
}

/** A recursive rewriting within the same rule.
 *
 *  @group Replacement
 */
case object RecursiveReplacement extends CaseReplacement {
  override def toString = "@(..)"
}
