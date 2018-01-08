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

  def inverse(k: K): K

}
