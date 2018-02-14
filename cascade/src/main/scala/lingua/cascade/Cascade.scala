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

class Cascade[In, Inner, Ctx, Out](head: Layer[In, Inner, Ctx], tail: Transformer[Inner, Out]) extends Transformer[In, Out] {

  def transform(s: Stream[In]): Stream[Out] =
    tail.transform(head.transform(s))

  def andThen[Out1](that: Transformer[Out, Out1]): Transformer[In, Out1] =
    new Cascade(head, tail.andThen(that))

}
