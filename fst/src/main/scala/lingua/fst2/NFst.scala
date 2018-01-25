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

import filter._

import scala.annotation.tailrec

import scala.collection.immutable.Queue

/** A non-deterministic Fst implementation. Multi-Fst cannot be represented by this Fst.
 *  A multi-Fst is an Fst for which there are several transitions from the same state for
 *  the same input symbol leading to the same target state.
 */
class NFst[In, Out](
    val states: Set[State],
    val initials: Set[State],
    val finals: Set[State],
    val transitions: Set[NTransition[In, Out]]) {

  private val (trans, outputs, eps) =
    transitions.foldLeft((Map.empty[(State, Option[In]), Set[State]], Map.empty[(State, Option[In], State), Option[Out]], false)) {
      case ((trans, outputs, eps), NTransition(src, in, out, tgt)) =>
        val trans1 = trans.updated((src, in), trans.getOrElse((src, in), Set.empty) + tgt)
        val outputs1 = outputs.updated((src, in, tgt), out)
        (trans1, outputs1, eps || in.isEmpty)
    }

  def hasEpsilonInput: Boolean = eps

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    initials.contains(state)

  def step(state: State, in: Option[In]): Set[State] =
    trans.getOrElse(state -> in, Set.empty)

  def output(origin: State, in: Option[In], target: State): Option[Out] =
    outputs.getOrElse((origin, in, target), None)

  def transitions(state: State): Set[NTransition[In, Out]] =
    transitions.filter(_.source == state)

  def accessibleStates: Set[State] = {
    val nexts =
      transitions.foldLeft(Map.empty[State, Set[State]]) {
        case (acc, NTransition(src, _, _, tgt)) =>
          acc.updated(src, acc.getOrElse(src, Set.empty) + tgt)
      }
    @tailrec
    def loop(from: Queue[State], visited: Set[State]): Set[State] =
      from.dequeueOption match {
        case Some((st, rest)) =>
          val nxts = nexts.getOrElse(st, Set.empty).diff(visited)
          loop(rest.enqueue(nxts), visited + st)
        case None =>
          visited
      }
    loop(Queue.empty ++ initials, initials)
  }

  def coaccessibleStates: Set[State] = {
    val previous =
      transitions.foldLeft(Map.empty[State, Set[State]]) {
        case (acc, NTransition(src, _, _, tgt)) =>
          acc.updated(tgt, acc.getOrElse(tgt, Set.empty) + src)
      }
    @tailrec
    def loop(from: Queue[State], visited: Set[State]): Set[State] =
      from.dequeueOption match {
        case Some((st, rest)) =>
          val prev = previous.getOrElse(st, Set.empty).diff(visited)
          loop(rest.enqueue(prev), visited + st)
        case None =>
          visited
      }
    loop(Queue.empty ++ finals, finals)
  }

  def compose[Out1](that: NFst[Out, Out1], filter: Filter[NTransition] = EpsilonSequencingFilter): NFst[In, Out1] = {
    val states = for {
      i1 <- this.states
      i2 <- that.states
    } yield (i1, i2, filter.initial)
    val init = states
    val queue = Queue.empty ++ states
    val mapping = states.foldLeft(Map.empty[(State, State, State), State]) { (acc, s) =>
      acc.updated(s, acc.size)
    }

    val allFinals =
      for {
        f1 <- this.finals
        f2 <- that.finals
        f3 <- filter.states
      } yield (f1, f2, f3)

    val el1 =
      (for (q <- this.states)
        yield q -> (this.transitions(q).map(t => t.copy(out = Option(t.out))) + NTransition(q, None, Some(None), q))).toMap

    val el2 =
      (for (q <- that.states)
        yield q -> (that.transitions(q).map(t => t.copy(in = Option(t.in))) + NTransition(q, Some(None), None, q))).toMap

    @tailrec
    def loop(queue: Queue[(State, State, State)], states: Set[(State, State, State)], finals: Set[(State, State, State)], transitions: Set[NTransition[In, Out1]], mapping: Map[(State, State, State), State]): NFst[In, Out1] =
      queue.dequeueOption match {
        case Some((q @ (q1, q2, q3), rest)) =>
          val finals1 =
            if (allFinals.contains(q))
              finals + q
            else
              finals

          val m =
            for {
              e1 <- el1(q1)
              e2 <- el2(q2)
              e @ (_, _, q31) = filter.step(e1, e2, q3)
              if q31 != filter.blocking
            } yield e

          val (states1, queue1, transitions1, mapping1) =
            m.foldLeft((states, queue, transitions, mapping)) {
              case ((states, queue, transitions, mapping), (e11, e21, q31)) =>
                val q = (e11.target, e21.target, q31)
                val (states1, queue1, mapping1) =
                  if (!states.contains(q))
                    (states + q, queue.enqueue(q), mapping.updated(q, mapping.size))
                  else
                    (states, queue, mapping)
                val transitions1 = transitions + NTransition(mapping1((q1, q2, q3)), e11.in, e21.out, mapping1(q))
                (states1, queue1, transitions1, mapping1)
            }
          loop(queue1, states1, finals1, transitions1, mapping1)
        case None =>
          new NFst(states.map(mapping(_)), init.map(mapping(_)), finals.map(mapping(_)), transitions)
      }
    loop(queue, states, Set.empty, Set.empty, mapping)
  }

  def determinize: PSubFst[In, Out] = {

    val initial = for (i <- initials) yield (i, Seq.empty[Out])

    val mapping = Map(initial -> 0)

    val queue = Queue(initial)

    val deferredNexts = epsilonReachable

    @tailrec
    def loop(finals: Set[State], transitions: Set[PTransition[In, Out]], finalOutputs: Map[State, Set[Seq[Out]]], queue: Queue[Set[(State, Seq[Out])]], mapping: Map[Set[(State, Seq[Out])], State]): PSubFst[In, Out] =
      queue.dequeueOption match {
        case Some((q2, rest)) =>
          val (finals1, finalOutputs1) =
            q2.foldLeft((finals, finalOutputs)) {
              case ((finals, finalOutputs), (q, w)) if this.finals.contains(q) =>
                val q1 = mapping(q2)
                (finals + q1, finalOutputs.updated(q1, finalOutputs.getOrElse(q1, Set.empty) + w))
              case (acc, _) =>
                acc
            }

          val outputs = q2.foldLeft(Map.empty[In, Set[(State, Seq[Out])]]) {
            case (acc, (q, outs)) =>
              deferredNexts(q).foldLeft(acc) {
                case (acc, (in, set)) =>
                  acc.updated(in, acc.getOrElse(in, Set.empty) ++ set.map { case (outs1, st) => (st, outs ++ outs1) })
              }
          }

          val (transitions1, mapping1, rest1) =
            outputs.foldLeft((transitions, mapping, rest)) {
              case ((transitions, mapping, rest), (in, set)) =>
                // compute the longest common prefix of all outgoing transitions
                // for this input character
                val l = lcp(set.map(_._2))
                val ls = l.size
                // the rest is the set of state with outputs without lcp
                val set1 = set.map { case (tgt, out) => (tgt, out.drop(ls)) }
                // if this results in a new state, then add it to the rest queue
                val (mapping1, rest1) =
                  if (rest.contains(set1))
                    (mapping, rest)
                  else
                    (mapping.updated(set1, mapping.size), rest :+ set1)
                // add the transition with accumulated prefix
                val transitions1 = transitions + PTransition(mapping1(q2), in, l, mapping1(set1))
                (transitions1, mapping1, rest1)
            }
          loop(finals1, transitions1, finalOutputs1, rest1, mapping1)
        case None =>
          new PSubFst(mapping.values.toSet, 0, finals, transitions, finalOutputs)
      }

    loop(Set.empty, Set.empty, Map.empty, queue, mapping)
  }

  private def epsilonReachable: Map[State, Map[In, Set[(Seq[Out], State)]]] =
    reverseTopologicalEpsilon.foldLeft(Map.empty[State, Map[In, Set[(Seq[Out], State)]]]) { (acc, state) =>
      val trans =
        transitions.foldLeft(Map.empty[In, Set[(Seq[Out], State)]]) {
          case (trans, NTransition(`state`, Some(in), out, tgt)) =>
            // add the transition to the accumulator for this state
            trans.updated(in, trans.getOrElse(in, Set.empty) + ((out.toSeq, tgt)))
          case (trans, NTransition(`state`, None, out, tgt)) =>
            // ask reachable elements from the acc for tgt
            // we know it as been treated because states are in reverse topological order
            // prepend the output of the epsilon transition
            acc(tgt).foldLeft(trans) {
              case (acc, (in, set)) =>
                trans.updated(in, trans.getOrElse(in, Set.empty) ++ set.map { case (out1, tgt) => (out.toSeq ++ out1, tgt) })
            }
          case (trans, _) =>
            trans
        }
      acc.updated(state, trans)
    }

  // returns the states in reverse topological order taking only epsilon transitions
  private def reverseTopologicalEpsilon: Seq[State] = {
    @tailrec
    def loop(todo: Seq[Seq[State]], temporary: Set[State], permanently: Set[State], acc: Seq[State]): Seq[State] = {
      todo match {
        case (hd +: tl) +: rest =>
          if (permanently(hd)) {
            // already done, go to next
            loop(tl +: rest, temporary, permanently, acc)
          } else if (temporary(hd)) {
            // cycle!!
            throw new IllegalArgumentException("This non-deterministic Fst cannot be determinized because it contains epsilon-cycles.")
          } else {
            // add this state to the result, and then process the next ones
            val nexts = step(hd, None)
            loop(nexts.toSeq +: todo, temporary + hd, permanently, hd +: acc)
          }
        case Seq() +: (hd +: tl) +: rest =>
          // all epsilon-reachable states of a state have been treated, it can be marked permanently
          loop(tl +: rest, temporary - hd, permanently + hd, acc)
        case Seq() +: Seq() =>
          // all done!
          acc
      }
    }

    loop(Seq(states.toSeq), Set.empty, Set.empty, Seq())
  }

}

object NFst {

  object Builder {

    def create[In, Out]: Builder[In, Out] =
      new Builder

  }

  class Builder[In, Out] private {

    import scala.collection.{ mutable => mu }

    private var nextStateId = 0

    private val transitions = mu.Set.empty[NTransition[In, Out]]

    private val finals = mu.Set.empty[Int]

    private val initials = mu.Set.empty[Int]

    class State private[NFst] (private val id: Int) {

      def addTransition(in: Option[In], out: Option[Out], target: State): this.type = {
        transitions += NTransition(id, in, out, target.id)
        this
      }

      def isInitial: Boolean =
        initials.contains(id)

      def isFinal: Boolean =
        finals.contains(id)

      def makeInitial: this.type = {
        initials += id
        this
      }

      def makeFinal: this.type = {
        finals += id
        this
      }

    }

    def newState: State = {
      val s = new State(nextStateId)
      nextStateId += 1
      s
    }

    def result: NFst[In, Out] =
      new NFst((0 until nextStateId).toSet, initials.toSet, finals.toSet, transitions.toSet)

  }

}
