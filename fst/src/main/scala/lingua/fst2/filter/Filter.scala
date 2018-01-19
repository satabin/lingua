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
package filter

import scala.language.higherKinds

trait Filter[T[_, _]] {

  def states: Set[State]

  def initial: State

  def blocking: State

  def step[In, Out1, Out2](t1: T[In, Option[Out1]], t2: T[Option[Out1], Out2], state: State): (T[In, Option[Out1]], T[Option[Out1], Out2], State)

}
