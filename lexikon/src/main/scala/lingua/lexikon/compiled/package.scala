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
package lingua.lexikon

package object compiled {

  type TransitionIndex = Long

  implicit class TransitionIndexOps(val ti: TransitionIndex) extends AnyVal {

    def char: Char =
      ((ti >>> 32) & 0xffff).toChar

    def transition: Int =
      (ti & 0xffffffff).toInt

  }

  object TransitionIndex {

    def unapply(ti: TransitionIndex): Option[(Char, Int)] =
      Some(ti.char -> ti.transition)

  }

}
