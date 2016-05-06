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

package object fst {

  type State = Int

  type Transition[In, Out] = (State, In, Seq[Out], State)

  def lcp[T](s1: Seq[T], s2: Seq[T]): Seq[T] =
    s1.zip(s2).takeWhile { case (t1, t2) => t1 == t2 }.unzip._1

  def lcp[T](ss: Iterable[Seq[T]]): Seq[T] =
    ss.foldLeft(None: Option[Seq[T]]) {
      case (None, s)      => Some(s)
      case (Some(s1), s2) => Some(lcp(s1, s2))
    }.getOrElse(Seq.empty[T])

  implicit class EpsilonNFstOps[In, Out](val nfst: NFst[Option[In], Out]) extends AnyVal {

    def removeEpsilonTransitions(): NFst[In, Out] = {

      // for each state, we merge the epsilon reachable targets with the following non epsilon transitions
      // also push the epsilon transition to final states, so that the originating state becomes final with the outputs
      val (newOutputs, newFinals) =
        nfst.states.foldLeft((Map.empty[(State, In, State), Seq[Out]], Map.empty[State, Set[Seq[Out]]])) {
          case (acc, state) =>
            val eps = epsReached(state, Set(state), Seq.empty)
            eps.foldLeft(acc) {
              case ((accOutputs, accFinals), (target, out)) =>
                // for each non epsilon transition, prepend out to the transition output
                val accOutputs1 = nfst.outputMap.foldLeft(accOutputs) {
                  case (accOutputs, ((`target`, Some(c), target2), targetOut)) =>
                    accOutputs.updated((state, c, target2), out ++ targetOut)
                  case (acc, _) =>
                    acc
                }
                val accFinals1 =
                  nfst.finals.get(target) match {
                    case Some(outs) =>
                      val prevStateOuts = nfst.finals.getOrElse(state, Set.empty)
                      accFinals.updated(state, prevStateOuts ++ outs.map(out ++ _))
                    case None =>
                      accFinals
                  }
                (accOutputs1, accFinals1)
            }
        }

      val newTransitions =
        for (((state, Some(c)), states) <- nfst.transitionMap)
          yield (state, c) -> states

      new NFst(nfst.states, nfst.initials, newFinals, newTransitions, newOutputs)
    }

    private def epsReached(state: State, viewed: Set[State], acc: Seq[Out]): Set[(State, Seq[Out])] =
      nfst.transitionMap.get(state -> None) match {
        case Some(targets) =>
          for {
            target <- targets
            if !viewed.contains(target)
            outputs = nfst.outputMap.getOrElse((state, None, target), Seq.empty)
            next <- epsReached(target, viewed + target, acc ++ outputs)
          } yield next
        case None =>
          Set(state -> acc)
      }

  }

}
