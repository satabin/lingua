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

/** A deterministic predicate-augemented Fst with queue implementation.
 *
 *  @author Lucas Satabin
 */
class QPFst[In, Out](states: Set[State],
    val initial: State,
    finals: Map[State, Set[Seq[Output[Out]]]],
    val transitions: Map[State, Seq[((Predicate[In], Boolean), Seq[Output[Out]], State)]]) extends Fst[QPFst, In, Output[Out]](states, Set(initial), finals) {

  def toDot: String = {
    val trans = for {
      (src, ts) <- transitions
      ((in, id), outs, tgt) <- ts
    } yield f"""q$src->q$tgt[label="${pred2string(in, id)}:${outs.mkString}"]"""
    toDot(trans)
  }

  private def pred2string[T](p: Predicate[T], id: Boolean) =
    if (id)
      f"⟨$p⟩"
    else
      p.toString

}

sealed trait Output[+Out]
final case class PredicateOutput[Out](p: Predicate[Out], identity: Boolean) extends Output[Out] {
  override def toString =
    if (identity)
      f"⟨$p⟩"
    else
      p.toString
}
case object PopOutput extends Output[Nothing] {
  override def toString = "⟨⟩"
}
