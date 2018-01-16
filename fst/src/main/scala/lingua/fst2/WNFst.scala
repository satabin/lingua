/* Copyright (c) 2017 Lucas Satabin
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

import semiring._
import ops._
import filter._

import scala.annotation.tailrec

import scala.collection.immutable.Queue

/** A non-deterministic wighted Fst implementation. Multi-Fst cannot be represented by this Fst.
 *  A multi-Fst is an Fst for which there are several transitions from the same state for
 *  the same input symbol leading to the same target state.
 */
class WNFst[In, Out, Weight](
    val states: Set[State],
    val initials: Set[State],
    val finals: Set[State],
    val transitions: Set[WTransition[In, Out, Weight]],
    val initialWeights: Map[State, Weight],
    val finalWeights: Map[State, Weight])(implicit semiring: Semiring[Weight]) extends Fst[Option[In], Out] {

  private val (trans, outputs, weights) =
    transitions.foldLeft((Map.empty[(State, Option[In]), Set[State]], Map.empty[(State, Option[In], State), Option[Out]], Map.empty[(State, Option[In], State), Weight])) {
      case ((trans, outputs, weights), WTransition(src, in, out, w, tgt)) =>
        val trans1 = trans.updated((src, in), trans.getOrElse((src, in), Set.empty) + tgt)
        val outputs1 = outputs.updated((src, in, tgt), out)
        val weights1 = weights.updated((src, in, tgt), w)
        (trans1, outputs1, weights1)
    }

  def step(state: State, in: Option[In]): Set[State] =
    trans.getOrElse(state -> in, Set.empty)

  def output(origin: State, in: Option[In], target: State): Option[Out] =
    outputs.getOrElse((origin, in, target), None)

  def weight(origin: State, in: Option[In], target: State): Weight =
    weights.getOrElse((origin, in, target), semiring.zero)

  def initialWeight(s: State): Weight =
    initialWeights.getOrElse(s, semiring.zero)

  def finalWeight(s: State): Weight =
    finalWeights.getOrElse(s, semiring.zero)

  def transitions(state: State): Set[WTransition[In, Out, Weight]] =
    transitions.filter(_.source == state)

  def compose[Out1](that: WNFst[Out, Out1, Weight], filter: WFilter[Weight] = new WEpsilonSequencingFilter): WNFst[In, Out1, Weight] = {
    val states = for {
      i1 <- this.states
      i2 <- that.states
    } yield (i1, i2, filter.initial)
    val init = states
    val queue = Queue.empty ++ states
    val mapping = states.foldLeft(Map.empty[(State, State, State), State]) { (acc, s) =>
      acc.updated(s, acc.size)
    }
    val initialWeights =
      for (q @ (q1, q2, i3) <- init)
        yield (mapping(q), this.initialWeights(q1) * that.initialWeights(q2))

    val allFinals =
      for {
        f1 <- this.finals
        f2 <- that.finals
        f3 <- filter.states
      } yield (f1, f2, f3)

    val el1 =
      (for (q <- this.states)
        yield q -> (this.transitions(q).map(t => t.copy(out = Option(t.out))) + WTransition(q, None, Some(None), semiring.one, q))).toMap

    val el2 =
      (for (q <- that.states)
        yield q -> (that.transitions(q).map(t => t.copy(in = Option(t.in))) + WTransition(q, Some(None), None, semiring.one, q))).toMap

    @tailrec
    def loop(queue: Queue[(State, State, State)], states: Set[(State, State, State)], finals: Set[(State, State, State)], transitions: Set[WTransition[In, Out1, Weight]], finalWeights: Map[State, Weight], mapping: Map[(State, State, State), State]): WNFst[In, Out1, Weight] =
      queue.dequeueOption match {
        case Some((q @ (q1, q2, q3), rest)) =>
          val (finals1, finalWeights1) =
            if (allFinals.contains(q) && filter.finalWeight(q3) != semiring.zero) {
              (finals + q, finalWeights.updated(mapping(q), this.finalWeight(q1) * that.finalWeight(q2) * filter.finalWeight(q3)))
            } else {
              (finals, finalWeights)
            }

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
                val transitions1 = transitions + WTransition(mapping1((q1, q2, q3)), e11.in, e21.out, e11.weight * e21.weight, mapping1(q))
                (states1, queue1, transitions1, mapping1)
            }
          loop(queue1, states1, finals1, transitions1, finalWeights1, mapping1)
        case None =>
          new WNFst(states.map(mapping(_)), init.map(mapping(_)), finals.map(mapping(_)), transitions, initialWeights.toMap, finalWeights)
      }
    loop(queue, states, Set.empty, Set.empty, Map.empty, mapping)
  }
}
