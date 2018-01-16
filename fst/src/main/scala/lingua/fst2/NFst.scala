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
    val transitions: Set[Transition[In, Out]]) extends Fst[Option[In], Out] {

  private val (trans, outputs) =
    transitions.foldLeft(Map.empty[(State, Option[In]), Set[State]] -> Map.empty[(State, Option[In], State), Option[Out]]) {
      case ((trans, outputs), Transition(src, in, out, tgt)) =>
        val trans1 = trans.updated((src, in), trans.getOrElse((src, in), Set.empty) + tgt)
        val outputs1 = outputs.updated((src, in, tgt), out)
        (trans1, outputs1)
    }

  def step(state: State, in: Option[In]): Set[State] =
    trans.getOrElse(state -> in, Set.empty)

  def output(origin: State, in: Option[In], target: State): Option[Out] =
    outputs.getOrElse((origin, in, target), None)

  def transitions(state: State): Set[Transition[In, Out]] =
    transitions.filter(_.source == state)

  def accessibleStates: Set[State] = {
    val nexts =
      transitions.foldLeft(Map.empty[State, Set[State]]) {
        case (acc, Transition(src, _, _, tgt)) =>
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
        case (acc, Transition(src, _, _, tgt)) =>
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

  def compose[Out1](that: NFst[Out, Out1], filter: Filter[Transition] = EpsilonSequencingFilter): NFst[In, Out1] = {
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
        yield q -> (this.transitions(q).map(t => t.copy(out = Option(t.out))) + Transition(q, None, Some(None), q))).toMap

    val el2 =
      (for (q <- that.states)
        yield q -> (that.transitions(q).map(t => t.copy(in = Option(t.in))) + Transition(q, Some(None), None, q))).toMap

    @tailrec
    def loop(queue: Queue[(State, State, State)], states: Set[(State, State, State)], finals: Set[(State, State, State)], transitions: Set[Transition[In, Out1]], mapping: Map[(State, State, State), State]): NFst[In, Out1] =
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
                val transitions1 = transitions + Transition(mapping1((q1, q2, q3)), e11.in, e21.out, mapping1(q))
                (states1, queue1, transitions1, mapping1)
            }
          loop(queue1, states1, finals1, transitions1, mapping1)
        case None =>
          new NFst(states.map(mapping(_)), init.map(mapping(_)), finals.map(mapping(_)), transitions)
      }
    loop(queue, states, Set.empty, Set.empty, mapping)
  }
}
