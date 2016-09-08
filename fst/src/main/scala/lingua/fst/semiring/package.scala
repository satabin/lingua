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
package lingua.fst

package object semiring {

  implicit object Log extends Semiring[Double] {
    val one = 0.0
    val zero = Double.PositiveInfinity
    def plus(k1: Double, k2: Double): Double =
      -math.log(math.exp(-k1) + math.exp(-k2))
    def times(k1: Double, k2: Double): Double =
      k1 + k2
    def inverse(k: Double): Double =
      -k
  }

  implicit object Probability extends Semiring[Double] {
    val one = 1.0
    val zero = 0.0
    def plus(k1: Double, k2: Double): Double =
      k1 + k2
    def times(k1: Double, k2: Double): Double =
      k1 * k2
    def inverse(k: Double): Double =
      1 / k
  }

  implicit object Tropical extends Semiring[Double] {
    val one = 0.0
    val zero = Double.PositiveInfinity
    def plus(k1: Double, k2: Double): Double =
      math.min(k1, k2)
    def times(k1: Double, k2: Double): Double =
      k1 + k2
    def inverse(k: Double): Double =
      -k
  }

}
