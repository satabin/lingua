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
package lingua.semiring

/** Semiring representation over elements of `K`.
 *
 *  @author Lucas Satabin
 */
trait Semiring[K] {

  def zero: K

  def one: K

  def plus(k1: K, k2: K): K

  def times(k1: K, k2: K): K

}

object Semiring {

  implicit object Log extends Semiring[Double] {
    val one = 0.0
    val zero = Double.PositiveInfinity
    def plus(k1: Double, k2: Double): Double =
      -math.log(math.exp(-k1) + math.exp(-k2))
    def times(k1: Double, k2: Double): Double =
      k1 + k2
  }

  implicit object Probability extends Semiring[Double] {
    val one = 1.0
    val zero = 0.0
    def plus(k1: Double, k2: Double): Double =
      k1 + k2
    def times(k1: Double, k2: Double): Double =
      k1 * k2
  }

  implicit object Tropical extends Semiring[Double] {
    val one = 0.0
    val zero = Double.PositiveInfinity
    def plus(k1: Double, k2: Double): Double =
      math.min(k1, k2)
    def times(k1: Double, k2: Double): Double =
      k1 + k2
  }

  implicit def String[T]: Semiring[Seq[T]] = new Semiring[Seq[T]] {
    val one = null
    val zero = Seq()
    def plus(s1: Seq[T], s2: Seq[T]): Seq[T] =
      if (s1 == null)
        s2
      else if (s2 == null)
        s1
      else
        s1.zip(s2).takeWhile { case (t1, t2) => t1 == t2 }.unzip._1
    def times(s1: Seq[T], s2: Seq[T]): Seq[T] =
      if (s1 == null || s2 == null)
        null
      else
        s1 ++ s2
  }

}

package object ops {

  implicit class SemiringOps[K](val self: K) extends AnyVal {

    def +(that: K)(implicit sem: Semiring[K]): K =
      sem.plus(self, that)

    def *(that: K)(implicit sem: Semiring[K]): K =
      sem.times(self, that)

  }

}
