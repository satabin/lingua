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

}
