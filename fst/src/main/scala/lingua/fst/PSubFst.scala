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

/** A p-subsequential finite-state transducer. */
class PSubFst[In, Out] private[fst] (states: Set[State],
  val initial: State,
  finals: Map[State, Set[Seq[Out]]],
  val transitions: Map[(State, In), State],
  val defaultTransitions: Map[State, State],
  val outputs: Map[(State, In), Seq[Out]],
  val defaultOutputs: Map[State, Seq[Out]])
    extends Fst(states, Set(initial), finals) {

  /** The value of p */
  def p =
    finals.map(_._2.size).max

  /** Returns the next state when reading `in` in state `state`.
   *  If no transition exists, returns `None`.
   */
  def delta(state: State, in: In): Option[State] =
    transitions.get((state, in)).orElse(defaultTransitions.get(state))

  /** Returns the state reached when reading word `ins` from state `state`.
   *  If at some point no transition can be found, returns `None`.
   */
  def delta(state: State, ins: Seq[In]): Option[State] =
    if (ins.isEmpty)
      Some(state)
    else
      delta(state, ins.head).flatMap(s => delta(s, ins.tail))

  /** Returns the output sequence encountered when reading `in` in state `state`. */
  def sigma(origin: State, in: In): Seq[Out] =
    outputs.get((origin, in)).orElse(defaultOutputs.get(origin)).getOrElse(Seq.empty)

  /** Returns the output sequence encountered when reading `ins` from state `state`. */
  def sigma(state: State, ins: Seq[In]): Seq[Out] =
    if (ins.isEmpty)
      Seq.empty[Out]
    else
      delta(state, ins.head) match {
        case Some(q) => sigma(state, ins.head) ++ sigma(q, ins.tail)
        case None    => Seq.empty[Out]
      }

  /** Returns the set of extra output associated to the state `state`. */
  def phi(state: State): Set[Seq[Out]] =
    finals.getOrElse(state, Set.empty)

  def toDot: String = {
    val trans = for {
      ((s1, in), s2) <- transitions
      out = outputs.getOrElse((s1, in), Seq()).mkString
    } yield f"""q$s1->q$s2[label="$in:$out"]"""
    toDot(trans)
  }

  /** pushes common prefixes in front when possible. */
  def push: PSubFst[In, Out] = {

    import scala.collection.{ mutable => mu }

    val prefixes = mu.Map.empty[State, Seq[Out]]

    val sigma2 = mu.Map.empty[(State, In), Seq[Out]] ++ outputs

    val phi2 = mu.Map.empty[State, Set[Seq[Out]]] ++ finals

    def computePrefix(state: State, seen: Set[State]): Unit =
      if (!prefixes.contains(state) && !seen.contains(state)) {
        // first compute the prefixes of next states
        for (((`state`, _), q) <- transitions)
          computePrefix(q, seen + state)

        val outs =
          if (isFinal(state))
            phi(state)
          else
            Set.empty[Seq[Out]]

        val prefix =
          lcp(outs ++ (for (((`state`, i), q) <- transitions) yield sigma(state, i) ++ (if (q != state) prefixes(q) else Seq.empty)))

        prefixes(state) = prefix

        // push it in front of all incoming edges
        if (isFinal(state))
          phi2(state) = for (o <- phi(state)) yield o.drop(prefix.size)
        else if (isInitial(state))
          for (((`state`, i), o) <- outputs) sigma2((state, i)) = (o ++ prefixes(transitions((state, i))))
        else
          for (((`state`, i), o) <- outputs) sigma2((state, i)) = (o ++ prefixes(transitions((state, i)))).drop(prefix.size)
      }

    computePrefix(initial, Set.empty[State])

    new PSubFst(states, initial, phi2.toMap, transitions, defaultTransitions, sigma2.toMap, defaultOutputs)

  }

}
