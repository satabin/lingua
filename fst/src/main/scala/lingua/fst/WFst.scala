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

abstract class WFst[In, Out, Weight: Semiring](val states: Set[State],
    val initials: Set[State],
    val finals: Map[State, (Weight, Set[Seq[Out]])]) {

  val semiring = implicitly[Semiring[Weight]]

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    initials.contains(state)

  def toDot(transitions: Iterable[String]): String = {
    f"""digraph {
       |  rankdir = LR
       |  ${
      states.map { s =>
        if (finals.contains(s))
          f"""q$s[shape=doublecircle, label=""];
             |  end$s[shape=plaintext,label=""];
             |  q$s->end$s[label="${finals(s)._2.map(_.mkString("[", ", ", "]")).mkString("\\n")} / ${finals(s)._1}"]""".stripMargin
        else
          f"""q$s[shape=circle,label=""]"""
      }.mkString(";\n  ")
    }
       |  ${transitions.mkString(";\n  ")}
       |}""".stripMargin
  }

}

