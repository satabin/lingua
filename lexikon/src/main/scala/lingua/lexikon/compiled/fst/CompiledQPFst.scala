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

import scala.collection.immutable.Queue

/** A compiled and compacted representation of a [[QPFst]].
 *
 *  Initial state is always at index 0 in the transition index array.
 *
 *  @author Lucas Satabin
 */
case class CompiledQPFst(alphabet: Vector[Char], outputs: Vector[Out], tia: ByteVector, ta: Vector[QPTransition], oa: Vector[Set[Seq[QPOutput]]]) extends CompiledFst {

  private val alphabetMap =
    alphabet.zipWithIndex.toMap

  /** Size of a state in bytes */
  private val stateSize =
    5 + 6 * alphabet.size

  /** Lookup for the word in this QPFst and returns the ordered sequence of outputs if it is found. */
  def lookup(word: String): Set[AnnotatedLemma] = {

    @tailrec
    def step(idx: Int, state: Int, queue: Queue[Char], acc: List[Out]): Set[AnnotatedLemma] =
      if (state >= 0) {
        if (idx >= word.size) {
          val isFinal = tia(state) == 1
          if (isFinal) {
            val outs = oa(tia.slice(state + 1, state + 5).toInt())
            if (outs.isEmpty) {
              val (root, anns) =
                acc.foldLeft((new StringBuilder, Set.empty[Annotation])) {
                  case ((root, anns), CharOut(c))    => (root.append(c), anns)
                  case ((root, anns), a: Annotation) => (root, anns + a)
                }
              Set(AnnotatedLemma(root.toString.reverse, anns))
            } else {
              val res =
                for (out <- outs) yield {
                  val (acc1, queue1) = out.foldLeft(acc -> queue) {
                    case ((acc, queue), QPOut(n)) =>
                      // add n-th output to acc
                      (outputs(n) :: acc, queue)
                    case ((acc, queue), QPIdentity) =>
                      // dequeue input character, find it in the outputs and add to acc
                      val (c, queue1) = queue.dequeue
                      (outputs(alphabetMap(c)) :: acc, queue1)
                    case ((acc, queue), QPPop) =>
                      // dequeue and forget
                      val (_, queue1) = queue.dequeue
                      (acc, queue1)
                  }
                  val (root, anns) =
                    acc1.foldLeft((new StringBuilder, Set.empty[Annotation])) {
                      case ((root, anns), CharOut(c))    => (root.append(c), anns)
                      case ((root, anns), a: Annotation) => (root, anns + a)
                      case (_, _)                        => throw new Exception("This should never happend, it is probably a BUG")
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
              val QPTransition(_, capture, out, target) = ta(trans)
              val queue1 = if (capture) queue.enqueue(c) else queue
              val (acc1, queue2) = out.foldLeft(acc -> queue1) {
                case ((acc, queue), QPOut(n)) =>
                  // add n-th output to acc
                  (outputs(n) :: acc, queue)
                case ((acc, queue), QPIdentity) =>
                  // dequeue input character, find it in the outputs and add to acc
                  val (c, queue1) = queue.dequeue
                  (outputs(alphabetMap(c)) :: acc, queue1)
                case ((acc, queue), QPPop) =>
                  // dequeue and forget
                  val (_, queue1) = queue.dequeue
                  (acc, queue1)
              }
              step(idx + 1, target, queue2, acc1)
            case _ =>
              Set.empty
          }
        }
      } else {
        Set.empty
      }

    step(0, 0, Queue(), Nil)
  }

}

object CompiledQPFst {
  def apply(fst: CompiledFst): Option[CompiledQPFst] = fst match {
    case fst @ CompiledQPFst(_, _, _, _, _) => Some(fst)
    case _                                  => None
  }
}
