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

import scala.language.higherKinds

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot prove that ${T} is a transition for inputs ${I} and outputs ${O}.")
trait Transition[T[_, _], I[_], O[_]] {

  def source[In, Out](t: T[In, Out]): State

  def in[In, Out](t: T[In, Out]): I[In]

  def out[In, Out](t: T[In, Out]): O[Out]

  def target[In, Out](t: T[In, Out]): State

}

object Transition {

  implicit class TransitionOps[T[_, _], I[_], O[_], In, Out](val t: T[In, Out]) extends AnyVal {

    @inline
    def source(implicit trans: Transition[T, I, O]): State =
      trans.source(t)

    @inline
    def in(implicit trans: Transition[T, I, O]): I[In] =
      trans.in(t)

    @inline
    def out(implicit trans: Transition[T, I, O]): O[Out] =
      trans.out(t)

    @inline
    def target(implicit trans: Transition[T, I, O]): State =
      trans.target(t)

  }

  def unapply[T[_, _], I[_], O[_], In, Out](t: T[In, Out])(implicit trans: Transition[T, I, O]): Option[(State, I[In], O[Out], State)] =
    Some((t.source, t.in, t.out, t.target))

}
