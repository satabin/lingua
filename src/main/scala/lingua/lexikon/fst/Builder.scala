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
package lexikon
package fst

import scala.collection.mutable.{
  Set,
  ListBuffer
}

object Builder {

  def create[In, Out]: Builder[In, Out] =
    new Builder[In, Out]

  implicit class FinalStateOp[In, Out, I](val state: StateBuilder[In, Out, I, FinalState]) extends AnyVal {

    def addOutput(out: Seq[Out]): StateBuilder[In, Out, I, FinalState] = {
      state.outputs += out
      state
    }

  }

  implicit class NoFinalStateOp[In, Out, I](val state: StateBuilder[In, Out, I, NoFinalState]) extends AnyVal {

    def makeFinal: StateBuilder[In, Out, I, FinalState] =
      new StateBuilder[In, Out, I, FinalState](state.id, state.transitions, state.outputs)
  }

  implicit class NoInitialStateOp[In, Out, F](val state: StateBuilder[In, Out, NoInitState, F]) extends AnyVal {

    def makeInitial: StateBuilder[In, Out, InitState, F] =
      new StateBuilder[In, Out, InitState, F](state.id, state.transitions, state.outputs)

  }

}

// whether a state is initial
sealed trait Init
sealed trait InitState extends Init
sealed trait NoInitState extends Init
// whether a state is final
sealed trait Final
sealed trait FinalState extends Final
sealed trait NoFinalState extends Final

class Builder[In, Out] private[fst] () {

  private var nextState = 0

  private[fst] val states = Set.empty[State]

  /** Creates a new state in this Fst and returns it */
  def newState: StateBuilder[In, Out, NoInitState, NoFinalState] = {
    val id = nextState
    nextState += 1
    states += id
    new StateBuilder[In, Out, NoInitState, NoFinalState](id)
  }

}

class StateBuilder[In, Out, Initial, Final] private[fst] (private[fst] val id: Int,
  private[fst] val transitions: ListBuffer[Transition[In, Out]] = ListBuffer.empty[Transition[In, Out]],
  private[fst] val outputs: Set[Seq[Out]] = Set.empty[Seq[Out]])
