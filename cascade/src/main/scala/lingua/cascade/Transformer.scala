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
package lingua
package cascade

/** A transformer is used to process stream and transform into
 *  another stream of data.
 */
abstract class Transformer[-In, +Out] {
  self =>

  /** Transforms the input stream into an output stream
   *  according to the semantics of this transformer.
   */
  def transform(s: Stream[In]): Stream[Out]

  def andThen[Out1](that: Transformer[Out, Out1]): Transformer[In, Out1]

  @inline
  def ~>:[In1, In2 <: In, Ctx](layer: Layer[In1, In2, Ctx]): Transformer[In1, Out] =
    layer.andThen(this)

}
