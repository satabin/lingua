/* Copyright (c) 2017 Lucas Satabin
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

import scala.language.higherKinds

/** Constraints an Fst must respect to be considered as such. */
abstract class Fst[F[_, _] <: Fst[F, _, _, _], In, Out, T] {

  type Transition = T

  def states: Set[State]

  def initials: Set[State]

  def finals: Set[State]

  def transitions: Set[Transition]

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    initials.contains(state)

  def compose[Out2](that: F[Out, Out2]): F[In, Out2]

}
