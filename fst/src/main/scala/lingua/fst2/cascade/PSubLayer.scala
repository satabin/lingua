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
package cascade

import matcher._
import runner._
import lingua.cascade._

import scala.annotation.tailrec

/** A layer wrapping a [[PSubFst p-subsequential Fst]].
 *  Fst accepting empty strings will not be considered has a match.
 */
abstract class PSubLayer[In, FstIn, FstOut, Out](pfst: RunnablePSubFst[FstIn, FstOut], matcher: Matcher[In, FstIn]) extends Layer[In, Out, (State, Seq[FstOut])] {

  override final protected def passThrough(ctx: Context, in: In): (Context, Seq[Out]) =
    (makeContext, passThrough(in))

  /** For each unmatched input element, how it is
   *  passed through to the output stream.
   */
  protected def passThrough(in: In): Seq[Out]

  /** For each input matched by the [[PSubFst]], there are up to `p`
   *  output sequences, reduce these to the output of the layer.
   */
  protected def reduce(ins: Seq[In], outs: Set[Seq[FstOut]]): Seq[Out]

  override final protected def reduce(ctx: Context, ins: Seq[In]): (Context, Seq[Out]) = {
    val (state, out) = ctx
    val finals = pfst.finalOutput(state)
    val outs =
      if (finals.isEmpty)
        Set(out)
      else
        finals.map(out ++ _)
    (makeContext, reduce(ins, outs))
  }

  final override def makeContext = (pfst.initial, Seq.empty)

  final override protected def one(ctx: Context, s: Stream[In]): (Context, Option[(Seq[In], Stream[In])]) = {
    @tailrec
    def loop(state: State, s: Stream[In], acc: Seq[In], accOut: Seq[FstOut], lastMatch: Option[(Context, Seq[In], Stream[In])]): (Context, Option[(Seq[In], Stream[In])]) =
      s match {
        case in #:: rest =>
          @tailrec
          def find(syms: Seq[FstIn]): Option[(FstIn, State)] =
            syms match {
              case Seq() =>
                None
              case Seq(sym, syms @ _*) =>
                pfst.step(state, sym) match {
                  case Some(state) => Some((sym, state))
                  case None        => find(syms)
                }
            }
          find(matcher.extract(in)) match {
            case Some((sym, state1)) =>
              val acc1 = acc :+ in
              val accOut1 = accOut ++ pfst.output(state, sym)
              val lastMatch1 =
                if (pfst.isFinal(state1))
                  Some(((state1, accOut1), acc1, rest))
                else
                  lastMatch
              loop(state1, rest, acc1, accOut1, lastMatch1)
            case None =>
              lastMatch match {
                case Some((ctx, in, rest)) => (ctx, Some((in, rest)))
                case _                     => ((state, accOut), None)
              }
          }
        case _ =>
          // stream has been exhausted
          lastMatch match {
            case Some((ctx, in, rest)) => (ctx, Some((in, rest)))
            case _                     => ((state, accOut), None)
          }
      }
    loop(ctx._1, s, Seq.empty, ctx._2, None)
  }

}
