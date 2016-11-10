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

sealed abstract class Predicate[-In] extends (In => Boolean) {

  def &&[In1 <: In](that: Predicate[In1]): Predicate[In1] =
    (this, that) match {
      case (EmptyPredicate, _) | (_, EmptyPredicate) => EmptyPredicate
      case (AnyPredicate, _)                         => that
      case (_, AnyPredicate)                         => this
      case (SetPredicate(s1, true), SetPredicate(s2, true)) =>
        val i = s1.intersect(s2)
        if (i.isEmpty)
          EmptyPredicate
        else
          SetPredicate(i, true)
      case (SetPredicate(s1, true), SetPredicate(s2, false)) =>
        val s = s1.filterNot(s2.contains(_))
        if (s.isEmpty)
          EmptyPredicate
        else
          SetPredicate(s, true)
      case (SetPredicate(s1, false), SetPredicate(s2, true)) =>
        val s = s2.filterNot(s1.contains(_))
        if (s.isEmpty)
          EmptyPredicate
        else
          SetPredicate(s, true)
      case (SetPredicate(s1, false), SetPredicate(s2, false)) =>
        SetPredicate(s1.union(s2), false)
    }

  def ||[In1 <: In](that: Predicate[In1]): Predicate[In1] =
    (this, that) match {
      case (EmptyPredicate, _)                              => that
      case (_, EmptyPredicate)                              => this
      case (AnyPredicate, _) | (_, AnyPredicate)            => AnyPredicate
      case (SetPredicate(s1, true), SetPredicate(s2, true)) => SetPredicate(s1.union(s2), true)
      case (SetPredicate(s1, true), SetPredicate(s2, false)) =>
        val d = s2.diff(s1)
        if (d.isEmpty)
          AnyPredicate
        else
          SetPredicate(d, false)
      case (SetPredicate(s1, false), SetPredicate(s2, true)) =>
        val d = s1.diff(s2)
        if (d.isEmpty)
          AnyPredicate
        else
          SetPredicate(d, false)
      case (SetPredicate(s1, false), SetPredicate(s2, false)) =>
        val i = s1.intersect(s2)
        if (i.isEmpty)
          AnyPredicate
        else
          SetPredicate(i, false)
    }

  def unary_! : Predicate[In] =
    this match {
      case EmptyPredicate     => AnyPredicate
      case AnyPredicate       => EmptyPredicate
      case SetPredicate(s, p) => SetPredicate(s, !p)
    }

}

object Predicate {

  def apply[T](v: T): Predicate[T] =
    SetPredicate(Set(v), true)

}

object NonEmptyPredicate {

  def unapply[T](p: Predicate[T]): Option[Predicate[T]] = p match {
    case EmptyPredicate => None
    case _              => Some(p)
  }

}

case object EmptyPredicate extends Predicate[Any] {
  def apply(a: Any) = false
  override def toString = "∅"
}

case object AnyPredicate extends Predicate[Any] {
  def apply(a: Any) = true
  override def toString = "★"
}

final case class SetPredicate[In](set: Set[In], positive: Boolean) extends Predicate[In] {
  def apply(i: In) =
    if (positive)
      set.contains(i)
    else
      !set.contains(i)
  override def toString = f"${if (!positive) "¬" else ""}${set.mkString("{", ", ", "}")}"
}
