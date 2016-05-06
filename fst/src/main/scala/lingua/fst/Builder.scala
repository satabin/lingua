/* Copyright (c) 2015 Lucas Satabin
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
package fst

import scala.collection.mutable.{
  Set,
  ListBuffer
}

object Builder {

  def create[In, Out]: Builder[In, Out] =
    new Builder[In, Out]

}

class Builder[In, Out] private[fst] () {

  private var nextState = 0

  private[fst] val states = Set.empty[StateBuilder[In, Out]]

  /** Creates a new state in this Fst and returns it */
  def newState: StateBuilder[In, Out] = {
    val id = nextState
    nextState += 1
    val b = new StateBuilder[In, Out](id)
    states += b
    b
  }

  private object InitState {
    def unapply(s: StateBuilder[_, _]): Option[State] =
      if (s.i)
        Some(s.id)
      else
        None
  }

  private object FinalState {
    def unapply(s: StateBuilder[_, _]): Option[State] =
      if (s.f)
        Some(s.id)
      else
        None
  }

  /** Builds a non-deterministic Fst with epsilon transitions. */
  def build(): NFst[Option[In], Out] = {
    import scala.collection.{ immutable => im }
    val initials =
      states.collect { case InitState(s) => s }.toSet
    val (trans, outs) =
      states.foldLeft((im.Map.empty[(State, Option[In]), im.Set[State]], im.Map.empty[(State, Option[In], State), Seq[Out]])) {
        case (acc, s) =>
          s.transitions.foldLeft(acc) {
            case ((trans, outs), (source, in, out, target)) =>
              val fromSource = trans.getOrElse((source, in), im.Set.empty[State])
              (trans + ((source, in) -> (fromSource + target)),
                outs + ((source, in, target) -> out))
          }
      }
    val finals =
      states.collect { case sb @ FinalState(s) => s -> sb.outputs.toSet }.toMap
    val states1 = states.map(_.id).toSet[State]
    new NFst(states1, initials, finals, trans, outs)

  }

}

class StateBuilder[In, Out] private[fst] (private[fst] val id: Int,
    private[fst] val transitions: ListBuffer[Transition[Option[In], Out]] = ListBuffer.empty[Transition[Option[In], Out]],
    private[fst] val outputs: Set[Seq[Out]] = Set.empty[Seq[Out]]) {

  var f = false
  var i = false

  def makeInitial: this.type = {
    i = true
    this
  }

  def makeFinal: this.type = {
    f = true
    this
  }

  def makeNonFinal: this.type = {
    f = false
    this
  }

  def addOutput(out: Seq[Out]): this.type = {
    outputs += out
    this
  }

  def addTransition(in: Option[In], out: Seq[Out], target: StateBuilder[In, Out]): this.type = {
    transitions.append((id, in, out, target.id))
    this
  }

}
