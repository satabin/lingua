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
package lingua.lexikon
package compiled

import scala.annotation.tailrec

/** A compiled and compacted representation of a p-subsequential Fst in a format inspired by the HFST
 *  runtime format (http://www.ling.helsinki.fi/~klinden/pubs/fsmnlp2009runtime.pdf).
 *  The fact that we know the Fst is p-subsequential allows us for several assumptions:
 *   - at most one transition per input character in a given state
 *   - no epsilon-transitions
 *  Each transition may output several values, so the table of transitions looks a bit different than
 *  the one in HFST runtime format.
 *
 *  States with a single outgoing transition are not present in the transition index array.
 *  Initial state is always at index 0 in the transition index array.
 *
 *
 *  @author Lucas Satabin
 */
case class CompiledPSubFst(alphabet: Vector[Char], outputs: Vector[Out], tia: Vector[TransitionIndex], ta: Vector[Transition]) {

  private val alphabetMap =
    alphabet.zipWithIndex.toMap

  /** Lookup for the word in this Fst and returns the ordered sequence of outputs if it is found. */
  def lookup(word: String): Option[Seq[Out]] = {

    @tailrec
    def step(idx: Int, state: Int, acc: List[Int]): Option[Seq[Out]] =
      if (idx >= word.size) {
        if ((state >= 0 && tia(state).isFinal) || (state < 0 && ta(state).isFinal))
          Some(acc.foldLeft(List.empty[Out]) { (acc, i) => outputs(i) :: acc })
        else
          None
      } else if (state >= 0) {
        val c = word(idx)
        val cidx = alphabetMap(c)
        tia(cidx) match {
          case TransitionIndex(`c`, trans) =>
            // there exists a transition for the read symbol, collect the output and goto target
            val Transition(_, out, target, _) = ta(trans)
            step(idx + 1, target, out.reverse_:::(acc))
          case _ =>
            None
        }
      } else {
        val c = word(idx)
        ta(-state) match {
          case Transition(`c`, out, target, _) =>
            step(idx + 1, target, out.reverse_:::(acc))
          case _ =>
            None
        }
      }

    step(0, 0, Nil)
  }

}
