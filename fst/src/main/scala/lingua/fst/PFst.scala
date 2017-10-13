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
package fst

/** A non-deterministic predicate-augmented Fst implementation.
 *  Implementation of the algorithms presented in _Finite State Transducers with Predicates and Identities_.
 *
 *  @author Lucas Satabin
 */
class PNFst[In, Out] private[fst] (states: Set[State],
    initials: Set[State],
    finals: Map[State, Set[Seq[Out]]],
    val transitions: Map[State, Seq[(Predicate[In], Seq[(Predicate[Out], Boolean)], State)]]) extends Fst[PNFst, In, Out](states, initials, finals) {

  def determinize: QPFst[In, Out] = {

    import scala.collection.{ mutable => mu }

    val pfinals = new mu.HashMap[State, mu.Set[Seq[Output[Out]]]] with mu.MultiMap[State, Seq[Output[Out]]]

    val ptransitions = mu.Map.empty[State, Seq[((Predicate[In], Boolean), Seq[Output[Out]], State)]]

    val queue = mu.Queue.empty[(State, P)]

    val initial = initials.map((_, false, Seq.empty[Output[Out]]))

    queue.enqueue((0 -> initial))

    val newStates = mu.Map[Set[State], State](initials -> 0)
    var nextStatedId = 1

    while (queue.nonEmpty) {

      val (pid, p) = queue.dequeue

      if (p.nonEmpty) {

        val trans = transitions(p)

        for ((pos, neg) <- splits(trans.keySet)) {
          // in the current P, is there any state for which a positive predicate was associated
          // to an identity transition in the original PNFst?
          val hasId = p.exists(p => hasIdentity(p._1, pos))
          val pred = neg.fold(pos.fold(AnyPredicate)(_ && _))(_ && !_)
          pred match {
            case NonEmptyPredicate(pred) =>
              // only add non empty transitions
              val tgts = pos.foldLeft(Set.empty[(State, Boolean, Seq[Output[Out]])]) {
                case (acc, p) =>
                  // if there was an identity, then for each state in p for which no identity
                  // existed in the original NFst, then append a pop output
                  if (hasId)
                    acc.union(trans(p).map { case (st, originalHasId, outs) => (st, originalHasId, if (originalHasId) outs else outs :+ PopOutput) })
                  else
                    acc.union(trans(p))
              }
              // compute the lcp of outputs
              val outLcp = lcp(tgts.map(_._3))
              // remove it from the new state set
              val newP = tgts.map { case (st, originalHasId, outs) => (st, originalHasId, outs.drop(outLcp.size)) }
              // get the new state identifier
              val tgtState =
                newStates.get(newP.map(_._1)) match {
                  case Some(id) => id
                  case None =>
                    val id = nextStatedId
                    nextStatedId += 1
                    newStates(newP.map(_._1)) = id
                    queue.enqueue((id, newP))
                    id
                }

              ptransitions(pid) = ptransitions.getOrElse(pid, Seq()) :+ (((pred, hasId), outLcp, tgtState))
            case _ => // ignore it
          }
        }

        // update the finals set for each final state in P
        for {
          (state, _, w) <- p
          outs <- finals.get(state)
          out <- if (outs.isEmpty) Set(Seq()) else outs
        } pfinals.addBinding(pid, w ++ out.map(o => PredicateOutput(Predicate(o), false)))

      }

    }

    new QPFst[In, Out](newStates.values.toSet, 0, pfinals.mapValues(_.toSet).toMap, ptransitions.toMap)

  }

  // type P represents the type of states in the determinized form of the PNFst
  // it is a set of states associate to outputs sequences and whether there was an identity
  // originally for these sequences
  private type P = Set[(State, Boolean, Seq[Output[Out]])]

  // given a set of PNFst states and sequence of output predicates, compute the output transitions
  // in the determinized corresponding state
  private def transitions(p: P): Map[Predicate[In], P] =
    p.foldLeft(Map.empty[Predicate[In], P]) {
      case (acc, (state, _, predsOut1)) =>
        transitions.getOrElse(state, Seq.empty).foldLeft(acc) {
          case (acc, (predIn, predsOut2, q)) =>
            val hasId = hasNIdentity(predsOut2)
            acc.updated(predIn, acc.getOrElse(predIn, Set.empty) + ((q, hasId, (predsOut1 ++ predsOut2.map { case (pred, id) => PredicateOutput(pred, id) }))))
        }
    }

  private def splits(p: Set[Predicate[In]]): Iterator[(Set[Predicate[In]], Set[Predicate[In]])] = {
    val ps = p.toSeq
    for {
      n <- Iterator.range(1, p.size + 1)
      pos <- ps.combinations(n)
    } yield (pos.toSet, p.diff(pos.toSet))
  }

  private def hasNIdentity(seq: Seq[(Predicate[Out], Boolean)]): Boolean =
    seq.exists(_._2)

  private def hasIdentity(seq: Seq[Output[Out]]): Boolean =
    seq.exists {
      case PredicateOutput(_, true) | PopOutput => true
      case _                                    => false
    }

  private def hasIdentity(state: State, ps: Set[Predicate[In]]): Boolean =
    transitions.getOrElse(state, Seq()).exists { case (p, outs, _) => ps.contains(p) && outs.exists(_._2) }

  def toDot: String = {
    val trans = for {
      (src, ts) <- transitions
      (in, outs, tgt) <- ts
    } yield f"""q$src->q$tgt[label="$in:${outs.map { case (p, i) => pred2string(p, i) }.mkString}"]"""
    toDot(trans)
  }

  private def pred2string[T](p: Predicate[T], id: Boolean) =
    if (id)
      f"⟨$p⟩"
    else
      p.toString

}

object PNFst {

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

    private[fst] val transitions: MultiMap[State, (Predicate[In], Seq[(Predicate[Out], Boolean)], State)] = new HashMap[State, Set[(Predicate[In], Seq[(Predicate[Out], Boolean)], State)]] with MultiMap[State, (Predicate[In], Seq[(Predicate[Out], Boolean)], State)]

    private[fst] val finals: MultiMap[State, Seq[Out]] = new HashMap[State, Set[Seq[Out]]] with MultiMap[State, Seq[Out]]

    /** Creates a new state in this Fst and returns it */
    def newState: StateBuilder[In, Out] = {
      val id = nextState
      nextState += 1
      val b = new StateBuilder[In, Out](this, id)
      states += b
      b
    }

    def build(): PNFst[In, Out] = {
      val nstates = states.map(_.id).toSet
      val ninitials = states.filter(_.i).map(_.id).toSet
      val nfinals = finals.mapValues(_.toSet).toMap
      val ntransitions = transitions.mapValues(_.toSeq).toMap
      new PNFst(nstates, ninitials, nfinals, ntransitions)
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
      if (!f) {
        f = true
        builder.finals(id) = Set()
      }
      this
    }

    def makeNonFinal: this.type = {
      if (f) {
        f = false
        builder.finals.remove(id)
      }
      this
    }

    def addFinalOutput(out: Seq[Out]): this.type = {
      assert(f, "Final outputs may only be added to final states")
      builder.finals.addBinding(id, out)
      this
    }

    def addTransition(in: Predicate[In], out: Seq[(Predicate[Out], Boolean)], target: StateBuilder[In, Out]): this.type = {
      builder.transitions.addBinding(id, (in, out, target.id))
      this
    }

  }

}
