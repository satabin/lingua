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

/** A non-deterministic Fst implementation. */
class NFst[In, Out] private (states: Set[State], initials: Set[State], finals: Map[State, Set[Seq[Out]]], maps: (Map[(State, In), Set[State]], Map[(State, In, State), Seq[Out]])) extends Fst(states, initials, finals) {

  def this(states: Set[State], initials: Set[State], finals: Map[State, Set[Seq[Out]]], transitions: Map[(State, In), Set[State]], outputs: Map[(State, In, State), Seq[Out]]) =
    this(states, initials, finals, (transitions, outputs))

  def this(states: Set[State], initials: Set[State], finals: Map[State, Set[Seq[Out]]], transitions: Set[Transition[In, Out]]) =
    this(states, initials, finals, transitions.foldLeft(
      (Map.empty[(State, In), Set[State]],
        Map.empty[(State, In, State), Seq[Out]])) {
        case ((transAcc, outAcc), (origin, input, output, target)) =>
          val transKey = (origin, input)
          val outKey = (origin, input, target)
          val transAcc1 =
            if (transAcc.contains(transKey))
              transAcc.updated(transKey, transAcc(transKey) + target)
            else
              transAcc.updated(transKey, Set(target))
          val outAcc1 = outAcc.updated(outKey, output)
          (transAcc1, outAcc1)
      })

  val (transitionMap, outputMap) = maps

  def delta(state: State, in: In): Set[State] =
    transitionMap.getOrElse((state, in), Set.empty)

  def sigma(origin: State, in: In, target: State): Seq[Out] =
    outputMap.getOrElse((origin, in, target), Seq.empty)

  def determinize: PSubFst[In, Out] = {

    import scala.collection.{ mutable => mu }

    val output2 = new mu.HashMap[State, mu.Set[Seq[Out]]] with mu.MultiMap[State, Seq[Out]]

    val delta2 = mu.Map.empty[(State, In), State]
    val sigma2 = mu.Map.empty[(State, In), Seq[Out]]

    val queue = mu.Queue.empty[(State, Set[(State, Seq[Out])])]

    val initial = initials.map((_, Seq.empty[Out]))

    queue.enqueue((0, initial))

    val newStates = mu.Map[Set[State], State](initial.map(_._1) -> 0)
    var nextStateId = 1

    while (queue.nonEmpty) {

      val (q2id, q2) = queue.dequeue

      def j1(a: In) =
        for {
          (st, b) <- transitionMap.keys
          if a == b
          (_, w) <- q2.find(_._1 == st)
        } yield (st, w)

      def j2(a: In) =
        for {
          (st, b) <- transitionMap.keys
          if a == b
          (_, w) <- q2.find(_._1 == st).toSet
          st1 <- delta(st, a)
        } yield (st, w, st1)

      for {
        (q, w) <- q2
        if this.finals.contains(q)
        out <- this.finals(q)
      } output2.addBinding(q2id, w ++ out)

      for {
        (q, w) <- q2
        (q_, a) <- transitionMap.keys
        if q == q_
      } {

        sigma2((q2id, a)) = lcp(
          for ((q, w) <- j1(a))
            yield w ++ lcp(
            for (q_ <- delta(q, a))
              yield sigma(q, a, q_)))

        val nextState =
          (for ((q, w, q_) <- j2(a))
            yield (q_, (w ++ sigma(q, a, q_)).drop(sigma2((q2id, a)).size))).toSet

        val nextId = newStates.get(nextState.map(_._1)) match {
          case Some(id) =>
            id
          case None =>
            newStates += (nextState.map(_._1) -> nextStateId)
            queue.enqueue((nextStateId, nextState))
            nextStateId += 1
            nextStateId - 1
        }

        delta2((q2id, a)) = nextId
      }

    }

    new PSubFst(newStates.values.toSet, 0, output2.mapValues(_.toSet).toMap, delta2.toMap, sigma2.toMap)
  }

  def toDot: String = {
    val trans = for {
      ((s1, in), ss2) <- transitionMap
      s2 <- ss2
      out = outputMap.getOrElse((s1, in, s2), Seq()).mkString
    } yield f"""q$s1->q$s2[label="$in:$out"]"""
    toDot(trans)
  }

}
