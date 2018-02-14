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

import scala.annotation.implicitNotFound

/** A proof that some input or output accepts epsilons and how to extract a non-epsilon value.
 *
 *  @author Lucas Satabin
 */
@implicitNotFound("Could not prove that ${EpsIn} accepts epsilon values")
trait EpsilonProof[EpsIn, NoEpsIn] {

  val Eps: EpsIn

  def unapplyNoEps(in: EpsIn): Option[NoEpsIn]

  def applyEps(in: NoEpsIn): EpsIn

}

object EpsilonProof {

  implicit def OptionEpsilon[T]: EpsilonProof[Option[T], T] = new EpsilonProof[Option[T], T] {
    val Eps = None
    def unapplyNoEps(o: Option[T]) = o
    def applyEps(t: T) = Some(t)
  }

}

object NoEps {

  @inline
  def unapply[EpsIn, NoEpsIn](in: EpsIn)(implicit proof: EpsilonProof[EpsIn, NoEpsIn]): Option[NoEpsIn] =
    proof.unapplyNoEps(in)

  @inline
  def apply[EpsIn, NoEpsIn](in: NoEpsIn)(implicit proof: EpsilonProof[EpsIn, NoEpsIn]): EpsIn =
    proof.applyEps(in)

}
