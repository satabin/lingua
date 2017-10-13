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

import semiring.Semiring

class WPSubFst[In, Out, Weight: Semiring] private[fst] (
    states: Set[State],
    initial: State,
    val initialWeight: Weight,
    finals: Map[State, Set[(Weight, Seq[Out])]],
    val transitions: Map[(State, In), State],
    val defaultTransitions: Map[State, State],
    val outputs: Map[(State, In), Seq[Out]],
    val defaultOutputs: Map[State, Seq[Out]],
    val weights: Map[(State, In), Weight],
    val defaultWeights: Map[State, Weight]) extends WFst(states, Map(initial -> initialWeight), finals)
