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
package fst2
package matcher

/** A matcher is used to match a concrete input value against
 *  a symbol from an Fst input alphabet.
 */
trait Matcher[In, Sym] {

  /** Extracts the symbols from the concrete input value.
   *  Symbols are matched in order, the first one that matches
   *  defines the transition that will be taken.
   */
  def extract(in: In): Seq[Sym]

  /** Matches the input against a symbol.
   */
  final def matches(in: In, sym: Sym): Boolean =
    extract(in).contains(sym)

}
