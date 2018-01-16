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

import semiring._

import scala.language.higherKinds

object EpsilonSequencingFilter extends EpsilonSequencingFilter[Transition]

class EpsilonSequencingFilter[T[_, _] <: TransitionLike[_, _]] extends Filter[T] {

  val states = Set(0, 1, -1)

  val initial = 0

  val blocking = -1

  def step[In, Out1, Out2](t1: T[In, Option[Out1]], t2: T[Option[Out1], Out2], state: State): (T[In, Option[Out1]], T[Option[Out1], Out2], State) =
    (t1.out, t2.in) match {
      case (Some(Some(x1)), Some(Some(x2))) if x1 == x2 =>
        (t1, t2, 0)
      case (Some(None), None) if state == 0 =>
        (t1, t2, 0)
      case (None, Some(None)) =>
        (t1, t2, 1)
      case _ =>
        (t1, t2, -1)
    }

}

class WEpsilonSequencingFilter[Weight](implicit sem: Semiring[Weight]) extends EpsilonSequencingFilter[WTransition[?, ?, Weight]] with WFilter[Weight] {

  def finalWeight(s: State): Weight = sem.one

}
