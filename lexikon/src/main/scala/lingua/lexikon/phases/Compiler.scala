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
package lexikon
package phases

import parser._

import compiled._

import fst.{
  PSubFst,
  State
}

import scala.collection.immutable.{
  VectorBuilder,
  TreeSet,
  Set => IMSet
}
import scala.collection.mutable.{
  ListBuffer,
  Map,
  Queue,
  Set
}

import scodec.bits._

class Compiler(fst: PSubFst[Char, Out], diko: Diko) extends Phase[CompileOptions, CompiledPSubFst](Some("compiler")) {

  def process(options: CompileOptions, reporter: Reporter): CompiledPSubFst = {

    val alphabetB = new VectorBuilder[Char]
    alphabetB ++= diko.alphabet
    alphabetB ++= diko.separators
    val alphabet = alphabetB.result

    val stateSize =
      5 + 6 * alphabet.size

    val outputsB = new VectorBuilder[Out]
    outputsB ++= diko.alphabet.map(CharOut(_))
    outputsB ++= diko.separators.map(CharOut(_))
    outputsB ++= diko.categories.map(c => CatOut(c.alias))
    outputsB ++= diko.tags.flatMap { t =>
      if (t.public)
        if (t.children.isEmpty)
          Some(TagOut(t.alias))
        else
          t.children.flatMap(t => if (t.public) Some(TagOut(t.alias)) else None)
      else
        None
    }
    val outputs = outputsB.result

    val ta = new VectorBuilder[Transition]
    var taSize = 0

    val oa = new VectorBuilder[IMSet[Seq[Int]]]
    var oaSize = 0

    val state2idx = Map.empty[State, Int]

    // worst case : all states have transitions for all characters in the alphabet
    // a 1 in the profile indicate that this byte is already used to encode some state,
    // each transition takes 6 bytes, the first one indicates finality of the state.
    var tiaProfile = BitVector.low(fst.states.size * stateSize)
    var tia = ByteVector.low(stateSize)

    // the first free byte in the profile
    var firstFree = 0
    var occupation = 0
    var base = 0

    val queue = Queue.empty[State]
    queue.enqueue(fst.initial)
    val processed = Set.empty[State]
    while (queue.nonEmpty) {
      val state = queue.dequeue
      processed += state

      // compute the transition indices for the state
      var ti = ByteVector.low(stateSize)

      for (outs <- fst.finals.get(state)) {
        ti = ti.update(0, 1)
        ti = ti.patch(1, ByteVector.fromInt(oaSize))
        oa += outs.map(_.map(outputs.indexOf(_)))
        oaSize += 1
      }

      var profile = BitVector.low(stateSize).patch(0, BitVector.high(5))
      occupation += 5

      for (((`state`, c), target) <- fst.transitionMap) {
        val cidx = alphabet.indexOf(c)
        ti = ti.patch(5 + cidx * 6, ByteVector.fromShort(c.toShort) ++ ByteVector.fromInt(taSize))
        profile = profile.patch(5 + cidx * 6, BitVector.high(6))
        occupation += 6

        ta += Transition(c, fst.outputMap(state -> c).map(outputs.indexOf(_)).toList, target)
        taSize += 1
        if (!processed.contains(target)) {
          queue.enqueue(target)
        }
      }

      // insert the transition indices into the tia at the first place that does not overlap anything
      var idx = firstFree
      var cont = true
      while (cont) {
        val slice = tiaProfile.slice(idx, idx + profile.size)
        if ((slice & profile) === BitVector.low(profile.size)) {
          cont = false
        } else {
          idx += 1
        }
      }

      state2idx(state) = idx
      tia = tia.padRight(math.max(tia.size, ti.size + idx)) | ti.padLeft(ti.size + idx).padRight(math.max(tia.size, ti.size + idx))
      tiaProfile |= profile.padLeft(profile.size + idx).padRight(tiaProfile.size)

      if (occupation * 100 / (tia.size - base) > options.occupation) {
        occupation = 0
        base = tia.size.toInt
        firstFree = base
      } else {
        firstFree = tiaProfile.indexOfSlice(BitVector.low(1), firstFree).toInt
      }

    }

    CompiledPSubFst(alphabet, outputs, tia, ta.result.map(t => t.copy(target = state2idx(t.target))), oa.result)
  }

}
