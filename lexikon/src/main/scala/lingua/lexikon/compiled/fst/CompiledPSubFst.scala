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
package fst

import scodec.bits._

import scala.annotation.tailrec

/** A compiled and compacted representation of a p-subsequential Fst in a format inspired by the HFST
 *  runtime format (http://www.ling.helsinki.fi/~klinden/pubs/fsmnlp2009runtime.pdf).
 *  The fact that we know the Fst is p-subsequential allows us for several assumptions:
 *   - at most one transition per input character in a given state
 *   - no epsilon-transitions
 *  Each transition may output several values, so the table of transitions looks a bit different than
 *  the one in HFST runtime format.
 *
 *  Initial state is always at index 0 in the transition index array.
 *
 *
 *  @author Lucas Satabin
 */
case class CompiledPSubFst(alphabet: Vector[Char], outputs: Vector[Out], tia: ByteVector, ta: Vector[PSubTransition], oa: Vector[Set[Seq[Int]]]) extends CompiledFst {

  private val alphabetMap =
    alphabet.zipWithIndex.toMap

  /** Size of a state in bytes */
  private val stateSize =
    5 + 6 * alphabet.size

  /** Lookup for the word in this Fst and returns the ordered sequence of outputs if it is found. */
  def lookup(word: String): Set[AnnotatedLemma] = {

    @tailrec
    def step(idx: Int, state: Int, acc: List[Int]): Set[AnnotatedLemma] =
      if (state >= 0) {
        val isFinal = tia(state) == 1
        if (idx >= word.size) {
          if (isFinal) {
            val outs = oa(tia.slice(state + 1, state + 5).toInt())
            if (outs.isEmpty) {
              val (root, anns) =
                acc.foldLeft((new StringBuilder, Set.empty[Annotation])) {
                  case ((root, anns), i) => outputs(i) match {
                    case CharOut(c)    => (root.append(c), anns)
                    case a: Annotation => (root, anns + a)
                  }
                }
              Set(AnnotatedLemma(root.toString.reverse, anns))
            } else {
              val res =
                for (out <- outs) yield {
                  val (root, anns) =
                    (out ++ acc).foldLeft((new StringBuilder, Set.empty[Annotation])) {
                      case ((root, anns), i) => outputs(i) match {
                        case CharOut(c)    => (root.append(c), anns)
                        case a: Annotation => (root, anns + a)
                      }
                    }
                  AnnotatedLemma(root.toString.reverse, anns)
                }
              res
            }
          } else {
            Set.empty
          }
        } else {
          val stateVector = tia.slice(state + 5, state + stateSize)
          val c = word(idx)
          val cidx = alphabetMap(c) * 6
          val ti = stateVector.slice(cidx, cidx + 6).toLong()
          ti match {
            case TransitionIndex(`c`, trans) =>
              // there exists a transition for the read symbol, collect the output and goto target
              val PSubTransition(_, out, target) = ta(trans)
              val acc1 = out.foldLeft(acc) {
                case (acc, -1) => alphabetMap(c) :: acc
                case (acc, n)  => n :: acc
              }
              step(idx + 1, target, acc1)
            case _ =>
              Set.empty
          }
        }
      } else {
        Set.empty
      }

    step(0, 0, Nil)
  }

}

object CompiledPSubFst {
  def apply(fst: CompiledFst): Option[CompiledPSubFst] = fst match {
    case fst @ CompiledPSubFst(_, _, _, _, _) => Some(fst)
    case _                                    => None
  }
}
