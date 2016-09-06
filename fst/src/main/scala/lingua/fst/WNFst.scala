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
package lingua.fst

/** A non-deterministic wieghted Fst implementation. Multi-Fst cannot be represented by this Fst.
 *  A multi-Fst is an Fst for which there are several transitions from the same state for
 *  the same input symbol leading to the same target state.
 *
 *  @author Lucas Satabin
 */
class WNFst[In, Out, Weight: Semiring] private[fst] (states: Set[State],
  initials: Set[State],
  finals: Map[State, (Weight, Set[Seq[Out]])],
  val transitions: Map[(State, In), Set[State]],
  val anyTransitions: Map[State, Set[State]],
  val outputs: Map[(State, In, State), Seq[Out]],
  val anyOutputs: Map[(State, State), Seq[Out]])
    extends WFst(states, initials, finals)

object WNFst {

  import scala.collection.mutable.{
    Set,
    Map,
    HashMap,
    MultiMap
  }

  object Builder {

    def create[In, Out, Weight: Semiring]: Builder[In, Out, Weight] =
      new Builder[In, Out, Weight]

  }

  class Builder[In, Out, Weight: Semiring] private[fst] () {

    val semiring = implicitly[Semiring[Weight]]

    private var nextState = 0

    private[fst] val states = Set.empty[StateBuilder[In, Out, Weight]]

    private[fst] val transitions: MultiMap[(State, Option[In]), State] = new HashMap[(State, Option[In]), Set[State]] with MultiMap[(State, Option[In]), State]
    private[fst] val anyTransitions: MultiMap[State, State] = new HashMap[State, Set[State]] with MultiMap[State, State]
    private[fst] val outputs = Map.empty[(State, Option[In], State), Seq[Out]]
    private[fst] val anyOutputs = Map.empty[(State, State), Seq[Out]]

    private[fst] var initialWeight: Weight = semiring.one
    private[fst] val weights = Map.empty[(State, Option[In], State), Weight]
    private[fst] val anyWeights = Map.empty[(State, State), Weight]

    private[fst] val finalOutputs: MultiMap[State, Seq[Out]] = new HashMap[State, Set[Seq[Out]]] with MultiMap[State, Seq[Out]]

    private[fst] val finalWeights = Map.empty[State, Weight].withDefaultValue(semiring.one)

    /** Creates a new state in this Fst and returns it */
    def newState: StateBuilder[In, Out, Weight] = {
      val id = nextState
      nextState += 1
      val b = new StateBuilder[In, Out, Weight](this, id)
      states += b
      b
    }

    def setInitialWeight(w: Weight): this.type = {
      initialWeight = w
      this
    }

    private object InitState {
      def unapply(s: StateBuilder[In, Out, Weight]): Option[State] =
        if (s.i)
          Some(s.id)
        else
          None
    }

    /** Builds a non-deterministic Fst with epsilon transitions. */
    def build(): WNFst[Option[In], Out, Weight] = {
      val initials =
        states.collect { case InitState(s) => s }.toSet
      val trans = transitions.mapValues(_.toSet).toMap
      val anyTrans = anyTransitions.mapValues(_.toSet).toMap
      val outs = outputs.toMap
      val anyOuts = anyOutputs.toMap
      val finals = finalOutputs.map { case (st, out) => (st, (finalWeights(st), out.toSet)) }.toMap
      val states1 = states.map(_.id).toSet
      new WNFst(states1, initials, finals, trans, anyTrans, outs, anyOuts)

    }

  }

  class StateBuilder[In, Out, Weight: Semiring] private[fst] (builder: Builder[In, Out, Weight], private[fst] val id: Int) {

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

    def addFinalOutput(out: Seq[Out]): this.type = {
      assert(f, "Final outputs may only be added to final states")
      builder.finalOutputs.addBinding(id, out)
      this
    }

    def setFinalWeight(w: Weight): this.type = {
      assert(f, "Final weights may only be added to final states")
      builder.finalWeights(id) = w
      this
    }

    def addTransition(in: Option[In], out: Seq[Out], w: Weight, target: StateBuilder[In, Out, Weight]): this.type = {
      import builder._
      assert(!(outputs.contains((id, in, target.id)) || anyOutputs.contains((id, target.id))), "Multi-Fsts are not allowed")
      transitions.addBinding((id, in), target.id)
      outputs((id, in, target.id)) = out
      weights((id, in, target.id)) = w
      this
    }

    def addAnyTransition(out: Seq[Out], w: Weight, target: StateBuilder[In, Out, Weight]): this.type = {
      import builder._
      assert(!(outputs.exists { case ((src, _, tgt), _) => src == id && tgt == target.id } || anyOutputs.contains((id, target.id))), "Multi-Fsts are not allowed")
      anyTransitions.addBinding(id, target.id)
      anyOutputs((id, target.id)) = out
      anyWeights((id, target.id)) = w
      this
    }

  }

}
