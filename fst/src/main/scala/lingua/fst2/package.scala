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

import scala.annotation.tailrec

package object fst2 {

  type State = Int

  def lcp[T](s1: Seq[T], s2: Seq[T]): Seq[T] =
    s1.zip(s2).takeWhile { case (t1, t2) => t1 == t2 }.unzip._1

  def lcp[T](ss: Iterable[Seq[T]]): Seq[T] =
    ss.foldLeft(None: Option[Seq[T]]) {
      case (None, s)      => Some(s)
      case (Some(s1), s2) => Some(lcp(s1, s2))
    }.getOrElse(Seq.empty[T])

  implicit def SeqOrdering[T: Ordering]: Ordering[Seq[T]] =
    new Ordering[Seq[T]] {
      def compare(seq1: Seq[T], seq2: Seq[T]): Int = {
        val size1 = seq1.size
        val size2 = seq2.size
        val size = math.min(size1, size2)
        @tailrec
        def loop(idx: Int): Int =
          if (idx >= size) {
            if (size1 == size2) {
              // both are equal
              0
            } else if (size2 > size1) {
              // first is prefix of second, then it is smaller
              -1
            } else {
              // second is prefix of first, then it is greater
              1
            }
          } else {
            val v1 = seq1(idx)
            val v2 = seq2(idx)
            val order = implicitly[Ordering[T]].compare(v1, v2)
            if (order == 0) {
              loop(idx + 1)
            } else if (order < 0) {
              -1
            } else {
              1
            }
          }
        loop(0)
      }
    }

}

