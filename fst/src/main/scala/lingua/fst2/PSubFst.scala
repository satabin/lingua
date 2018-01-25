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
