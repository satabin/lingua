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

/** A compiled transition for [[QPFst]].
 *  The `out` sequence contains the indices of outputs in the outputs array of the compiled Fst,
 *  or a pop identity from the stack or a pop and forget from the stack.
 *
 *  @author Lucas Satabin
 */
final case class QPTransition(in: Char, capture: Boolean, out: List[QPOutput], target: Int)

sealed trait QPOutput
final case class QPOut(idx: Int) extends QPOutput
case object QPIdentity extends QPOutput
case object QPPop extends QPOutput
