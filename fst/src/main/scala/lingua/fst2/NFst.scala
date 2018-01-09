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
    val transitions: Set[(State, Option[In], Option[Out], State)]) extends Fst[NFst, In, Out, (State, Option[In], Option[Out], State)] {

  private val (trans, outputs) =
    transitions.foldLeft(Map.empty[(State, Option[In]), Set[State]] -> Map.empty[(State, Option[In], State), Option[Out]]) {
      case ((trans, outputs), (src, in, out, tgt)) =>
        val trans1 = trans.updated((src, in), trans.getOrElse((src, in), Set.empty) + tgt)
        val outputs1 = outputs.updated((src, in, tgt), out)
        (trans1, outputs1)
    }

  def step(state: State, in: Option[In]): Set[State] =
    trans.getOrElse(state -> in, Set.empty)

  def outputs(origin: State, in: Option[In], target: State): Option[Out] =
    outputs.getOrElse((origin, in, target), None)

  def transitions(state: State): Set[Transition] =
    transitions.filter(_._1 == state)

  def compose[Out2](that: NFst[Out, Out2]): NFst[In, Out2] = {
    val i1i2 =
      for {
        i1 <- this.initials
        i2 <- that.initials
      } yield (i1, i2)

    val f1f2 =
      for {
        f1 <- this.finals
        f2 <- that.finals
      } yield (f1, f2)

    val states = i1i2

    val queue = Queue.empty ++ i1i2

    val mapping = states.foldLeft(Map.empty[(State, State), State]) {
      case (acc, q) => acc.updated(q, acc.size)
    }

    @tailrec
    def loop(init: Set[(State, State)], finals: Set[(State, State)], states: Set[(State, State)], queue: Queue[(State, State)], trans: Set[(State, Option[In], Option[Out2], State)], mapping: Map[(State, State), State]): NFst[In, Out2] =
      queue.dequeueOption match {
        case Some((q @ (q1, q2), rest)) =>
          val init1 =
            if (i1i2.contains(q))
              init + q
            else
              init

          val finals1 =
            if (f1f2.contains(q))
              finals + q
            else
              finals

          val e1e2 =
            for {
              e1 @ (_, _, o1, _) <- this.transitions(q1)
              e2 @ (_, i2, _, _) <- that.transitions(q2)
              if o1 == i2
            } yield (e1, e2)

          val (states1, queue1, trans1, mapping1) =
            e1e2.foldLeft((states, queue, trans, mapping)) {
              case ((states, queue, trans, mapping), ((src1, in1, out1, tgt1), (src2, in2, out2, tgt2))) =>
                val (states1, queue1, mapping1) =
                  if (queue.contains((tgt1, tgt2)))
                    (states, queue, mapping)
                  else
                    (states + (tgt1 -> tgt2), queue.enqueue(tgt1 -> tgt2), mapping.updated(tgt1 -> tgt2, mapping.size))
                (states1, queue1, trans + ((mapping1(q), in1, out2, mapping1(tgt1 -> tgt2))), mapping1)
            }

          loop(init1, finals1, states1, queue1, trans1, mapping1)

        case None =>
          new NFst(states.map(mapping(_)), init.map(mapping(_)), finals.map(mapping(_)), trans)
      }

    loop(Set.empty, Set.empty, states, queue, Set.empty, mapping)
  }

  def accessibleStates: Set[State] = {
    val nexts =
      transitions.foldLeft(Map.empty[State, Set[State]]) {
        case (acc, (src, _, _, tgt)) =>
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
        case (acc, (src, _, _, tgt)) =>
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

}
