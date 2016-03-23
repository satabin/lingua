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
package lingua
package fst

import scodec._
import scodec.codecs._
import scodec.bits._
import scodec.codecs.literals._

import shapeless.HNil

case class Header(numOfInSym: Int,
  numOfSym: Int,
  sizeOfTransitionIndexTable: Int,
  sizeOfTransitionTargetTable: Int,
  numOfStates: Int,
  numOfTransitions: Int,
  weighted: Boolean,
  deterministic: Boolean,
  inputDeterministic: Boolean,
  minimized: Boolean,
  cyclic: Boolean,
  hasEpsEpsTransitions: Boolean,
  hasInputEpsTransitions: Boolean,
  hasInputEpsCycles: Boolean,
  hasUnweightedInputEspCylces: Boolean)

case class Alphabet(keyTable: Vector[String],
  operations: Map[Int, FlagDiacriticOperation],
  features: Int)

trait FlagDiacriticOperation

/** A codec that allows for reading and writing transducers in the HFST-runtime format.
 *  This is a port to scala of the java implementation found at https://github.com/hfst/hfst-optimized-lookup/ by Sam Harwick.
 *
 *  @author Lucas Satabin
 */
class FstCodec {

  val utf8Nul: Codec[String] = filtered(utf8, new Codec[BitVector] {
    val nul = BitVector.lowByte
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(bits: BitVector): Attempt[BitVector] = Attempt.successful(bits ++ nul)
    override def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] = {
      bits.bytes.indexOfSlice(nul.bytes) match {
        case -1 => Attempt.failure(Err("Does not contain a 'NUL' termination byte."))
        case i  => Attempt.successful(DecodeResult(bits.take(i * 8L), bits.drop(i * 8L + 8L)))
      }
    }
  }).withToString("utf8Nul")

  val header: Codec[Header] =
    (("number_of_input_symbols" | uint16L) ::
      ("number_of_symbols" | uint16L) ::
      ("size_of_transition_index_table" | uint32L.xmap[Int](_.toInt, _.toLong)) ::
      ("size_of_transition_target_table" | uint32L.xmap[Int](_.toInt, _.toLong)) ::
      ("number_of_states" | uint32L.xmap[Int](_.toInt, _.toLong)) ::
      ("number_of_transitions" | uint32L.xmap[Int](_.toInt, _.toLong)) ::
      ("weighted" | bool(32)) ::
      ("deterministic" | bool(32)) ::
      ("input_deterministic" | bool(32)) ::
      ("minimized" | bool(32)) ::
      ("cyclic" | bool(32)) ::
      ("has_epsilon_epsilon_transitions" | bool(32)) ::
      ("has_input_epsilon_transitions" | bool(32)) ::
      ("has_input_epsilon_cycles" | bool(32)) ::
      ("has_unweighted_input_epsilon_cycles" | bool(32))).as[Header]

  def alphabet(numOfSym: Int): Codec[_ <: Any] =
    ???

}
