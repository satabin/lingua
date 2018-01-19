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

case class WTransition[In, Out, Weight](source: State, in: Option[In], out: Option[Out], weight: Weight, target: State)

object WTransition {
  implicit def WTransitionTransition[Weight]: Transition[WTransition[?, ?, Weight], Option, Option] = new Transition[WTransition[?, ?, Weight], Option, Option] {
    type I[T] = Option[T]
    type O[T] = Option[T]
    def in[In, Out](t: WTransition[In, Out, Weight]): Option[In] = t.in
    def out[In, Out](t: WTransition[In, Out, Weight]): Option[Out] = t.out
    def source[In, Out](t: WTransition[In, Out, Weight]): State = t.source
    def target[In, Out](t: WTransition[In, Out, Weight]): State = t.target
  }
}
