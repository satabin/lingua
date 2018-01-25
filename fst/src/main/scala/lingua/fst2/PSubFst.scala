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

import filter._

import scala.annotation.tailrec

import scala.collection.immutable.Queue

import cats.Show
import cats.implicits._

/** A p-subsequential Fst implementation.
 */
class PSubFst[In, Out](
    val states: Set[State],
    val initial: State,
    val finals: Set[State],
    val transitions: Set[PTransition[In, Out]],
    val finalOutputs: Map[State, Set[Seq[Out]]]) {

  val p = math.max(1, finalOutputs.values.map(_.size).max)

  private val (trans, outputs) =
    transitions.foldLeft(Map.empty[(State, In), State] -> Map.empty[(State, In), Seq[Out]]) {
      case ((trans, outputs), PTransition(src, in, out, tgt)) =>
        val trans1 = trans.updated((src, in), tgt)
        val outputs1 = outputs.updated((src, in), out)
        (trans1, outputs1)
    }

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    state == initial

  def step(state: State, in: In): Option[State] =
    trans.get(state -> in)

  def output(state: State, in: In): Seq[Out] =
    outputs.getOrElse(state -> in, Seq.empty)

}

object PSubFst {
  object Builder {

    /** Builds a minimal p-subsequential Fst from a sequence of ordered entries.
     *  This is an implementation of the algorithm described in `Direct Construction of Minimal Acyclic Subsequential Transducers`
     *  by Mihov and Maurel.
     */
    def fromEntries[In <% Seq[I]: Ordering, Out <% Seq[O], I: Ordering, O](entries: Seq[(In, Out)]): PSubFst[I, O] =
      fromEntries[I, O](entries.map { case (i, o) => (i.toSeq, o.toSeq) })

    /** Builds a minimal p-subsequential Fst from a sequence of ordered entries.
     *  This is an implementation of the algorithm described in `Direct Construction of Minimal Acyclic Subsequential Transducers`
     *  by Mihov and Maurel.
     */
    def fromEntries[In: Ordering, Out](entries: Seq[(Seq[In], Seq[Out])]): PSubFst[In, Out] = {

      var _id = 0
      def nextId = {
        _id += 1
        _id - 1
      }
      case class State(transitions: Map[In, (Seq[Out], State)] = Map.empty[In, (Seq[Out], State)], isFinal: Boolean = false, stateOutput: Set[Seq[Out]] = Set.empty[Seq[Out]]) {

        private val id: Int = nextId

        def setTransition(c: In, tgt: State): State =
          copy(transitions = transitions.updated(c, (transitions.get(c).map(_._1).getOrElse(Seq.empty), tgt)))

        def setOutput(c: In, out: Seq[Out]): State =
          copy(transitions = transitions.updated(c, (out, transitions(c)._2)))

        def setFinal(f: Boolean): State =
          copy(isFinal = f)

        def setStateOutput(out: Set[Seq[Out]]): State =
          copy(stateOutput = out)

        def showTransitions(sb: StringBuilder): Unit = {
          transitions.foreach {
            case (in, (out, target)) =>
              sb.append(f"  q$id->q${target.id}[label=<$in:$out>]\n")
              target.showTransitions(sb)
          }
          if (isFinal) {
            sb.append(f"  q$id->end[label=<${stateOutput.mkString("\n")}>]\n")
          }
        }

      }

      // entries are sorted by inputs
      val sorted = entries.sortBy(_._1)

      // the input alphabet and maximal word size
      val (alphabet, maxWordSize) =
        entries.foldLeft((Set.empty[In], 0)) {
          case ((alphabet, maxWordSize), (in, _)) =>
            (alphabet ++ in, math.max(maxWordSize, in.size))
        }

      val firstIn = alphabet.min
      val lastIn = alphabet.max

      def getStateId(st: State, nextStateId: Int, current: Map[State, Int]): (Int, Int, Map[State, Int]) =
        current.get(st) match {
          case Some(id) => (id, nextStateId, current)
          case None     => (nextStateId, nextStateId + 1, current.updated(st, nextStateId))
        }

      @tailrec
      def mkPSubFst(nextStateId: Int, current: Map[State, Int], treated: Set[State], toProcess: Queue[State], states: Set[Int], finals: Map[Int, Set[Seq[Out]]], transitions: Set[PTransition[In, Out]]): PSubFst[In, Out] =
        toProcess.dequeueOption match {
          case Some((st, rest)) =>
            val (id, nextStateId1, current1) = getStateId(st, nextStateId, current)
            val states1 = states + id
            val finals1 =
              if (st.isFinal)
                finals.updated(id, st.stateOutput)
              else
                finals
            val (rest1, nextStateId2, current2, transitions1) =
              st.transitions.foldLeft((rest, nextStateId1, current1, transitions)) {
                case ((rest, nextStateId, current, transitions), (in, (out, nxt))) =>
                  val (nxtId, nextStateId1, current1) = getStateId(nxt, nextStateId, current)
                  val rest1 =
                    if (treated.contains(nxt))
                      rest
                    else
                      rest.enqueue(nxt)
                  (rest1, nextStateId1, current1, transitions + PTransition(id, in, out, nxtId))
              }
            mkPSubFst(nextStateId2, current2, treated + st, rest1, states1, finals1, transitions1)
          case None =>
            new PSubFst(states, 0, finals.keySet, transitions, finals)
        }

      @tailrec
      def loop(entries: Seq[(Seq[In], Seq[Out])], previous: Seq[In], tempStates: Vector[State], minTransducerStates: Set[State]): PSubFst[In, Out] = {

        def findMinimized(st: State): (State, Set[State]) =
          minTransducerStates.find(_ == st) match {
            case Some(st) => (st, minTransducerStates)
            case None     => (st, minTransducerStates + st)
          }

        entries match {
          case Seq() =>
            val (tempStates1, minTransducerStates1) =
              (previous.size to 1 by -1).foldLeft((tempStates, minTransducerStates)) {
                case ((tempStates, minTransducerStates), i) =>
                  val (st, minTransducerStates1) = findMinimized(tempStates(i))
                  (tempStates.updated(i - 1, tempStates(i - 1).setTransition(previous(i - 1), st)), minTransducerStates1)
              }
            mkPSubFst(0, Map.empty, Set.empty, Queue(tempStates1(0)), Set.empty, Map.empty, Set.empty)
          case Seq((current, currentOut), rest @ _*) =>
            val prefixSizePlus1 = previous.zip(current).takeWhile(p => p._1 == p._2).size + 1

            // minimize states from from suffix of previous word
            val (tempStates1, minTransducerStates1) =
              (previous.size to prefixSizePlus1 by -1).foldLeft((tempStates, minTransducerStates)) {
                case ((tempStates, minTransducerStates), i) =>
                  val (st, minTransducerStates1) = findMinimized(tempStates(i))
                  (tempStates.updated(i - 1, tempStates(i - 1).setTransition(previous(i - 1), st)), minTransducerStates1)
              }

            // initialize tail states for current word
            val tempStates2 =
              (prefixSizePlus1 to current.size).foldLeft(tempStates1) { (tempStates, i) =>
                val tempStates1 = tempStates.updated(i, State())
                tempStates1.updated(i - 1, tempStates1(i - 1).setTransition(current(i - 1), tempStates1(i)))
              }

            // mark the last state as final if this is not the same word as the previous one
            val tempStates3 =
              if (current != previous)
                tempStates2.updated(current.size, tempStates2(current.size).setFinal(true))
              else
                tempStates2

            val (currentOut1, tempStates4) =
              (1 until prefixSizePlus1).foldLeft((currentOut, tempStates3)) {
                case ((currentOut, tempStates), i) =>
                  val tOut = tempStates(i - 1).transitions(current(i - 1))._1
                  val commonPrefix = lcp(tOut, currentOut)
                  val wordSuffix = tOut.drop(commonPrefix.size)
                  val tempStates1 = tempStates.updated(i - 1, tempStates(i - 1).setOutput(current(i - 1), commonPrefix))

                  // push the suffix to the end
                  val tempStates2 =
                    tempStates1(i).transitions.keySet.foldLeft(tempStates1) { (tempStates, in) =>
                      val st = tempStates(i)
                      val out = st.transitions(in)._1
                      tempStates.updated(i, st.setOutput(in, wordSuffix ++ out))
                    }

                  val tempStates3 =
                    if (tempStates2(i).isFinal) {
                      val stateOutput1 =
                        if (tempStates2(i).stateOutput.nonEmpty)
                          tempStates2(i).stateOutput.map(out => wordSuffix ++ out)
                        else
                          Set(wordSuffix)
                      tempStates2.updated(i, tempStates2(i).setStateOutput(stateOutput1))
                    } else {
                      tempStates2
                    }

                  val currentOut1 = currentOut.drop(commonPrefix.size)
                  (currentOut1, tempStates3)

              }

            val tempStates5 =
              if (current == previous) {
                val st = tempStates4(current.size)
                tempStates4.updated(current.size, st.setStateOutput(st.stateOutput + currentOut1))
              } else {
                val idx = math.min(prefixSizePlus1, current.size)
                val st = tempStates4(idx - 1)
                tempStates4.updated(idx - 1, st.setOutput(current(idx - 1), currentOut1))
              }

            loop(rest, current, tempStates5, minTransducerStates1)
        }
      }

      loop(sorted, Seq.empty, Vector.fill(maxWordSize + 1)(State()), Set.empty)
    }

  }

  implicit def PSubFstDotShow[In, Out](implicit inShow: Show[In], outShow: Show[Out]): Show[PSubFst[In, Out]] = new Show[PSubFst[In, Out]] {

    def show(pfst: PSubFst[In, Out]): String = {
      val builder = new StringBuilder
      builder.append("""digraph {
                         |  rankdir = LR
                         |""".stripMargin)

      for (state <- pfst.states) {
        builder.append("  q").append(state).append("[shape=")
        if (pfst.isFinal(state))
          builder.append("doublecircle")
        else
          builder.append("circle")
        builder.append(", label=\"\"];\n")
        if (pfst.isInitial(state))
          builder
            .append("  start")
            .append(state)
            .append("[shape=plaintext, label=\"\"];\n  start")
            .append(state)
            .append("->q")
            .append(state)
            .append(";\n")
        for {
          (out, idx) <- pfst.finalOutputs.getOrElse(state, Set.empty).zipWithIndex
          if out.nonEmpty
        } builder
          .append("  end")
          .append(state)
          .append("_")
          .append(idx)
          .append("[shape=plaintext, label=\"\"];\n  q")
          .append(state)
          .append("->end")
          .append(state)
          .append("_")
          .append(idx)
          .append("[label=\"")
          .append(out.map(_.show).mkString)
          .append("\"];\n")
      }

      builder.append("\n")

      for (PTransition(src, in, out, tgt) <- pfst.transitions) {
        builder
          .append("  q")
          .append(src)
          .append("->q")
          .append(tgt)
          .append("[label=\"")
          .append(in.show)
          .append(":")
          .append(out.map(_.show).mkString)
          .append("\"];\n")
      }

      builder.append("}")

      builder.toString
    }

  }

}
