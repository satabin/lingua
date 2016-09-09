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

/** A non-deterministic Fst implementation. Multi-Fst cannot be represented by this Fst.
 *  A multi-Fst is an Fst for which there are several transitions from the same state for
 *  the same input symbol leading to the same target state.
 *
 *  @author Lucas Satabin
 */
class NFst[In, Out] private[fst] (states: Set[State],
  initials: Set[State],
  finals: Map[State, Set[Seq[Out]]],
  val transitions: Map[(State, In), Set[State]],
  val anyTransitions: Map[State, Set[State]],
  val outputs: Map[(State, In, State), Seq[Out]],
  val anyOutputs: Map[(State, State), Seq[Out]])
    extends Fst[NFst, In, Out](states, initials, finals) {

  def delta(state: State, in: In): Set[State] =
    transitions.getOrElse((state, in), Set.empty) ++ anyTransitions.getOrElse(state, Set.empty)

  def sigma(origin: State, in: In, target: State): Seq[Out] =
    outputs.get((origin, in, target)).orElse(anyOutputs.get((origin, target))).getOrElse(Seq.empty)

  /** Provided we can prove that this non-deterministic Fst accepts epsilon transitions, returns
   *  the equivalent non-deterministic Fst without any epsilon transitions.
   */
  def removeEpsilonTransitions[In1](implicit epsilonProof: EpsilonProof[In, In1]): NFst[In1, Out] = {

    import epsilonProof._

    def epsReached(state: State, viewed: Set[State], acc: Seq[Out]): Set[(State, Seq[Out])] =
      transitions.get(state -> Eps) match {
        case Some(targets) =>
          for {
            target <- targets
            if !viewed.contains(target)
            outputs = this.outputs.getOrElse((state, Eps, target), Seq.empty)
            next <- epsReached(target, viewed + target, acc ++ outputs)
          } yield next
        case None =>
          Set(state -> acc)
      }

    // for each state, we merge the epsilon reachable targets with the following non epsilon transitions
    // also push the epsilon transition to final states, so that the originating state becomes final with the outputs
    val (newAnyOutputs, newOutputs, newFinals) =
      states.foldLeft((Map.empty[(State, State), Seq[Out]], Map.empty[(State, In1, State), Seq[Out]], Map.empty[State, Set[Seq[Out]]])) {
        case (acc, state) =>
          val eps = epsReached(state, Set(state), Seq.empty)
          eps.foldLeft(acc) {
            case ((accAnyOutputs, accOutputs, accFinals), (target, out)) =>
              // for each non epsilon transition, prepend out to the transition output
              val accOutputs1 = outputs.foldLeft(accOutputs) {
                case (accOutputs, ((`target`, NoEps(c), target2), targetOut)) =>
                  accOutputs.updated((state, c, target2), out ++ targetOut)
                case (acc, _) =>
                  acc
              }
              val accFinals1 =
                finals.get(target) match {
                  case Some(outs) =>
                    val prevStateOuts = finals.getOrElse(state, Set.empty)
                    accFinals.updated(state, prevStateOuts ++ outs.map(out ++ _))
                  case None =>
                    accFinals
                }

              val accAnyOutputs1 = anyOutputs.foldLeft(accAnyOutputs) {
                case (accAnyOutputs, ((`target`, target2), targetOut)) =>
                  accAnyOutputs.updated((state, target2), out ++ targetOut)
                case (acc, _) =>
                  acc
              }
              (accAnyOutputs1, accOutputs1, accFinals1)
          }
      }

    val newTransitions =
      for (((state, NoEps(c)), states) <- transitions)
        yield (state, c) -> states

    new NFst(states, initials, newFinals, newTransitions, anyTransitions, newOutputs, newAnyOutputs)
  }

  /** Build the determinisitc version of this non-deterministic Fst. */
  def determinize: PSubFst[In, Out] = {

    import scala.collection.{ mutable => mu }

    val pfinals = new mu.HashMap[State, mu.Set[Seq[Out]]] with mu.MultiMap[State, Seq[Out]]

    val ptransitions = mu.Map.empty[(State, In), State]
    val panyTransitions = mu.Map.empty[State, State]
    val poutputs = mu.Map.empty[(State, In), Seq[Out]]
    val panyOutputs = mu.Map.empty[State, Seq[Out]]

    val queue = mu.Queue.empty[(State, Set[(State, Seq[Out])])]

    val initial = initials.map((_, Seq.empty[Out]))

    queue.enqueue((0, initial))

    val newStates = mu.Map[Set[State], State](initial.map(_._1) -> 0)
    var nextStateId = 1

    while (queue.nonEmpty) {

      val (q2id, q2) = queue.dequeue

      def j1(a: In) =
        for {
          (st, `a`) <- transitions.keys
          (_, w) <- q2.find(_._1 == st)
        } yield (st, w)

      lazy val anyj1 =
        for {
          st <- anyTransitions.keys
          (_, w) <- q2.find(_._1 == st)
        } yield (st, w)

      def j2(a: In) =
        for {
          (st, `a`) <- transitions.keys
          (_, w) <- q2.find(_._1 == st).toSet
          st1 <- delta(st, a)
        } yield (st, w, st1)

      lazy val anyj2 =
        for {
          (st, reached) <- anyTransitions
          (_, w) <- q2.find(_._1 == st).toSet
          st1 <- reached
        } yield (st, w, st1)

      for {
        (q, w) <- q2
        if this.finals.contains(q)
        out <- this.finals(q)
      } pfinals.addBinding(q2id, w ++ out)

      for ((q, _) <- q2) {
        for ((`q`, a) <- transitions.keys) {
          poutputs((q2id, a)) = lcp(
            for ((q, w) <- j1(a))
              yield w ++ lcp(
              for (q_ <- delta(q, a))
                yield sigma(q, a, q_)))

          val nextState =
            (for ((q, w, q_) <- j2(a))
              yield (q_, (w ++ sigma(q, a, q_)).drop(poutputs((q2id, a)).size))).toSet

          val nextId = newStates.get(nextState.map(_._1)) match {
            case Some(id) =>
              id
            case None =>
              newStates += (nextState.map(_._1) -> nextStateId)
              queue.enqueue((nextStateId, nextState))
              nextStateId += 1
              nextStateId - 1
          }

          ptransitions((q2id, a)) = nextId
        }

        for (`q` <- anyTransitions.keys) {
          panyOutputs(q2id) = lcp(
            for ((q, w) <- anyj1)
              yield w ++ lcp(
              for (q_ <- anyTransitions.getOrElse(q, Set.empty))
                yield anyOutputs((q, q_))))

          val nextState =
            (for ((q, w, q_) <- anyj2)
              yield (q_, (w ++ anyOutputs((q, q_))).drop(panyOutputs(q2id).size))).toSet

          val nextId = newStates.get(nextState.map(_._1)) match {
            case Some(id) =>
              id
            case None =>
              newStates += (nextState.map(_._1) -> nextStateId)
              queue.enqueue((nextStateId, nextState))
              nextStateId += 1
              nextStateId - 1
          }

          panyTransitions(q2id) = nextId
        }
      }
    }

    new PSubFst(newStates.values.toSet, 0, pfinals.mapValues(_.toSet).toMap, ptransitions.toMap, panyTransitions.toMap, poutputs.toMap, panyOutputs.toMap)
  }

  def toDot: String = {
    val trans = for {
      ((s1, in), ss2) <- transitions
      s2 <- ss2
      out = outputs.getOrElse((s1, in, s2), Seq()).mkString
    } yield f"""q$s1->q$s2[label="$in:$out"]"""
    toDot(trans)
  }

}

object NFst {

  import scala.collection.mutable.{
    Set,
    Map,
    HashMap,
    MultiMap
  }

  object Builder {

    def create[In, Out]: Builder[In, Out] =
      new Builder[In, Out]

  }

  class Builder[In, Out] private[fst] () {

    private var nextState = 0

    private[fst] val states = Set.empty[StateBuilder[In, Out]]

    private[fst] val transitions: MultiMap[(State, Option[In]), State] = new HashMap[(State, Option[In]), Set[State]] with MultiMap[(State, Option[In]), State]
    private[fst] val anyTransitions: MultiMap[State, State] = new HashMap[State, Set[State]] with MultiMap[State, State]
    private[fst] val outputs = Map.empty[(State, Option[In], State), Seq[Out]]
    private[fst] val anyOutputs = Map.empty[(State, State), Seq[Out]]

    private[fst] val finalOutputs: MultiMap[State, Seq[Out]] = new HashMap[State, Set[Seq[Out]]] with MultiMap[State, Seq[Out]]

    /** Creates a new state in this Fst and returns it */
    def newState: StateBuilder[In, Out] = {
      val id = nextState
      nextState += 1
      val b = new StateBuilder[In, Out](this, id)
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
      val trans = transitions.mapValues(_.toSet).toMap
      val anyTrans = anyTransitions.mapValues(_.toSet).toMap
      val outs = outputs.toMap
      val anyOuts = anyOutputs.toMap
      val finals = finalOutputs.mapValues(_.toSet).toMap
      val states1 = states.map(_.id).toSet
      new NFst(states1, initials, finals, trans, anyTrans, outs, anyOuts)

    }

  }

  class StateBuilder[In, Out] private[fst] (builder: Builder[In, Out], private[fst] val id: Int) {

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

    def addTransition(in: Option[In], out: Seq[Out], target: StateBuilder[In, Out]): this.type = {
      import builder._
      assert(!(outputs.contains((id, in, target.id)) || anyOutputs.contains((id, target.id))), "Multi-Fsts are not allowed")
      transitions.addBinding((id, in), target.id)
      outputs((id, in, target.id)) = out
      this
    }

    def addAnyTransition(out: Seq[Out], target: StateBuilder[In, Out]): this.type = {
      import builder._
      assert(!(outputs.exists { case ((src, _, tgt), _) => src == id && tgt == target.id } || anyOutputs.contains((id, target.id))), "Multi-Fsts are not allowed")
      anyTransitions.addBinding(id, target.id)
      anyOutputs((id, target.id)) = out
      this
    }

  }
}
