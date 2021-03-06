/* Copyright (c) 2018 Lucas Satabin
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
package fst2

case class NTransition[In, Out](source: State, in: Option[In], out: Option[Out], target: State)

object NTransition {
  implicit object NTransitionTransition extends Transition[NTransition, Option, Option] {
    def in[In, Out](t: NTransition[In, Out]): Option[In] = t.in
    def out[In, Out](t: NTransition[In, Out]): Option[Out] = t.out
    def source[In, Out](t: NTransition[In, Out]): State = t.source
    def target[In, Out](t: NTransition[In, Out]): State = t.target
  }
}
