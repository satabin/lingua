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

import compiled.fst._

import scodec.Attempt

import better.files._

import java.nio.file.StandardOpenOption

import compiled.fst._

import fst._

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

import scala.reflect._

class Serializer(files: Seq[GeneratedFile], diko: Diko) extends Phase[CompileOptions, Unit](Some("serializer")) {

  def process(options: CompileOptions, reporter: Reporter): Unit = {
    options.outputDir.createDirectories
    for (file <- files) file match {
      case DotFile(file, content) =>
        file.overwrite(content)
      case PSubFstFile(file, fst) =>
        FstProtocol.file.encode(compile(fst, options)) match {
          case Attempt.Successful(bytes) =>
            for (raf <- file.newFileChannel(Seq(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)).autoClosed) {
              raf.write(bytes.toByteBuffer)
            }
          case Attempt.Failure(err) =>
            reporter.error(err.toString)
        }
      case QPFstFile(file, fst) =>
        val compiled = compile(fst, options, reporter)
        if (!reporter.hasErrors)
          FstProtocol.file.encode(compiled) match {
            case Attempt.Successful(bytes) =>
              for (raf <- file.newFileChannel(Seq(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)).autoClosed) {
                raf.write(bytes.toByteBuffer)
              }
            case Attempt.Failure(err) =>
              reporter.error(err.toString)
          }
    }
  }

  private lazy val (alphabet, outputs, stateSize) = {
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
    (alphabet, outputs, stateSize)
  }

  private val CharSet = classTag[IMSet[Char]]
  private def enumerate(pred: Predicate[Char]): IMSet[Char] = pred match {
    case AnyPredicate                      => alphabet.toSet
    case EmptyPredicate                    => IMSet()
    case SetPredicate(CharSet(set), true)  => set
    case SetPredicate(CharSet(set), false) => alphabet.toSet.diff(set)
  }

  private def toCompiedOutputs(out: Output[Out], reporter: Reporter) = out match {
    case PredicateOutput(AnyPredicate, true) => QPIdentity
    case PredicateOutput(SetPredicate(Singleton(c), true), false) => QPOut(outputs.indexOf(c))
    case PopOutput => QPPop
    case out =>
      reporter.error(f"Unsupported output $out")
      QPPop
  }

  private def compile(fst: QPFst[Char, Out], options: CompileOptions, reporter: Reporter) = {

    val ta = new VectorBuilder[QPTransition]
    var taSize = 0

    val oa = new VectorBuilder[IMSet[Seq[QPOutput]]]
    var oaSize = 0

    val state2idx = Map.empty[State, Int]

    var tia = ByteVector.low(0)

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
        oa += outs.map(_.map(toCompiedOutputs(_, reporter)))
        oaSize += 1
      }

      for {
        ((pred, capture), out, target) <- fst.transitions(state)
        c <- enumerate(pred)
      } {
        val cidx = alphabet.indexOf(c)
        if (cidx < 0)
          println(f"$c -> $cidx")
        ti = ti.patch(5 + cidx * 6, ByteVector.fromShort(c.toShort) ++ ByteVector.fromInt(taSize))

        ta += QPTransition(c, capture, out.map(toCompiedOutputs(_, reporter)).toList, target)
        taSize += 1
        if (!processed.contains(target)) {
          queue.enqueue(target)
        }
      }

      state2idx(state) = tia.size.toInt
      tia ++= ti

    }

    CompiledQPFst(alphabet, outputs, tia, ta.result.map(t => t.copy(target = state2idx(t.target))), oa.result)
  }

  private def compile(fst: PSubFst[Char, Out], options: CompileOptions) = {

    val ta = new VectorBuilder[PSubTransition]
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

      fst.defaultTransitions.get(state) match {

        case Some(dfltTarget) =>
          // there is a default transition, store it as a dense transition in the end of the current chunk and create a new chunk
          // add the default transition
          val dfltTrans = taSize
          ta += PSubTransition(0, fst.defaultOutputs(state).map(outputs.indexOf(_)).toList, dfltTarget)
          taSize += 1
          if (!processed.contains(dfltTarget)) {
            queue.enqueue(dfltTarget)
          }
          // dense states start at the end of the current transition index array
          val idx = tia.size.toInt
          state2idx(state) = idx

          for (c <- alphabet) {
            fst.transitions.get(state -> c) match {
              case Some(target) =>
                val cidx = alphabet.indexOf(c)
                ti = ti.patch(5 + cidx * 6, ByteVector.fromShort(c.toShort) ++ ByteVector.fromInt(taSize))

                ta += PSubTransition(c, fst.outputs(state -> c).map(outputs.indexOf(_)).toList, target)
                taSize += 1
                if (!processed.contains(target)) {
                  queue.enqueue(target)
                }
              case None =>
                // use the default transition
                val cidx = alphabet.indexOf(c)
                ti = ti.patch(5 + cidx * 6, ByteVector.fromShort(c.toShort) ++ ByteVector.fromInt(dfltTrans))
            }
          }

          tia ++= ti
          tiaProfile ++= BitVector.high(stateSize)

          occupation = 0
          base = tia.size.toInt
          firstFree = base

        case None =>
          // no default transition for this state, store the sparse transition using the first-fit algorithm
          var profile = BitVector.low(stateSize).patch(0, BitVector.high(5))
          occupation += 5

          for (((`state`, c), target) <- fst.transitions) {
            val cidx = alphabet.indexOf(c)
            ti = ti.patch(5 + cidx * 6, ByteVector.fromShort(c.toShort) ++ ByteVector.fromInt(taSize))
            profile = profile.patch(5 + cidx * 6, BitVector.high(6))
            occupation += 6

            ta += PSubTransition(c, fst.outputs(state -> c).map(outputs.indexOf(_)).toList, target)
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
    }

    CompiledPSubFst(alphabet, outputs, tia, ta.result.map(t => t.copy(target = state2idx(t.target))), oa.result)
  }

}
