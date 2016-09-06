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
  val initialWeight: Weight,
  finals: Map[State, (Weight, Set[Seq[Out]])],
  val transitions: Map[(State, In), Set[State]],
  val anyTransitions: Map[State, Set[State]],
  val outputs: Map[(State, In, State), Seq[Out]],
  val anyOutputs: Map[(State, State), Seq[Out]],
  val weights: Map[(State, In, State), Weight],
  val anyWeights: Map[(State, State), Weight])
    extends WFst(states, initials, finals) {

  def delta(state: State, in: In): Set[State] =
    transitions.getOrElse((state, in), Set.empty) ++ anyTransitions.getOrElse(state, Set.empty)

  def sigma(origin: State, in: In, target: State): Seq[Out] =
    outputs.get((origin, in, target)).orElse(anyOutputs.get((origin, target))).getOrElse(Seq.empty)

  /** Provided we can prove that this non-deterministic Fst accepts epsilon transitions, returns
   *  the equivalent non-deterministic Fst without any epsilon transitions.
   */
  def removeEpsilonTransitions[In1](implicit epsilonProof: EpsilonProof[In, In1]): WNFst[In1, Out, Weight] = {

    import epsilonProof._

    def epsReached(state: State, viewed: Set[State], accOut: Seq[Out], accW: Weight): Set[(State, Seq[Out], Weight)] =
      transitions.get(state -> Eps) match {
        case Some(targets) =>
          for {
            target <- targets
            if !viewed.contains(target)
            outputs = this.outputs.getOrElse((state, Eps, target), Seq.empty)
            w = this.weights.getOrElse((state, Eps, target), semiring.one)
            next <- epsReached(target, viewed + target, accOut ++ outputs, semiring.times(accW, w))
          } yield next
        case None =>
          Set((state, accOut, accW))
      }

    // for each state, we merge the epsilon reachable targets with the following non epsilon transitions
    // also push the epsilon transition to final states, so that the originating state becomes final with the outputs
    val (newAnyOutputs, newOutputs, newAnyWeights, newWeights, newFinals) =
      states.foldLeft((Map.empty[(State, State), Seq[Out]], Map.empty[(State, In1, State), Seq[Out]], Map.empty[(State, State), Weight], Map.empty[(State, In1, State), Weight], Map.empty[State, (Weight, Set[Seq[Out]])])) {
        case (acc, state) =>
          val eps = epsReached(state, Set(state), Seq.empty, semiring.one)
          eps.foldLeft(acc) {
            case ((accAnyOutputs, accOutputs, accAnyWeights, accWeights, accFinals), (target, out, weight)) =>
              // for each non epsilon transition, prepend out to the transition output
              val accOutputs1 = outputs.foldLeft(accOutputs) {
                case (accOutputs, ((`target`, NoEps(c), target2), targetOut)) =>
                  accOutputs.updated((state, c, target2), out ++ targetOut)
                case (acc, _) =>
                  acc
              }
              val accWeights1 = weights.foldLeft(accWeights) {
                case (accWeights, ((`target`, NoEps(c), target2), targetW)) =>
                  accWeights.updated((state, c, target2), semiring.times(weight, targetW))
                case (acc, _) =>
                  acc
              }
              val accFinals1 =
                finals.get(target) match {
                  case Some((fweight, outs)) =>
                    val (prevStateW, prevStateOuts) = finals.getOrElse(state, (semiring.zero, Set.empty[Seq[Out]]))
                    accFinals.updated(state, (semiring.plus(prevStateW, semiring.times(weight, fweight)), prevStateOuts ++ outs.map(out ++ _)))
                  case None =>
                    accFinals
                }

              val accAnyOutputs1 = anyOutputs.foldLeft(accAnyOutputs) {
                case (accAnyOutputs, ((`target`, target2), targetOut)) =>
                  accAnyOutputs.updated((state, target2), out ++ targetOut)
                case (acc, _) =>
                  acc
              }
              val accAnyWeights1 = anyWeights.foldLeft(accAnyWeights) {
                case (accAnyWeights, ((`target`, target2), targetW)) =>
                  accAnyWeights.updated((state, target2), semiring.times(weight, targetW))
                case (acc, _) =>
                  acc
              }
              (accAnyOutputs1, accOutputs1, accAnyWeights1, accWeights1, accFinals1)
          }
      }

    val newTransitions =
      for (((state, NoEps(c)), states) <- transitions)
        yield (state, c) -> states

    new WNFst(states, initials, initialWeight, newFinals, newTransitions, anyTransitions, newOutputs, newAnyOutputs, newWeights, newAnyWeights)
  }

}

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
      val weights1 = weights.toMap
      val anyWeights1 = anyWeights.toMap
      new WNFst(states1, initials, initialWeight, finals, trans, anyTrans, outs, anyOuts, weights1, anyWeights1)

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
