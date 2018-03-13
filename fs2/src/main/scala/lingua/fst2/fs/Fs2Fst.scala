package lingua.fst2
package fs

import runner._
import matcher._

import fs2._

import scala.annotation.tailrec

import scala.language.higherKinds

/** A fs2 pipe that uses the underlying [[RunnablePSubFst]] to process input.
 */
abstract class Fs2PSubFst[In, FstIn, FstOut, Out](val pfst: RunnablePSubFst[FstIn, FstOut], matcher: Matcher[In, FstIn]) {

  /** Passes through an element that is not part of a recognized input. */
  protected def passThrough(in: In): Segment[Out, Unit]

  /** Reduces the outputs of recognized input to output elements.
   *  The matching input sequence is given together with the Fst outputs.
   */
  protected def reduce(ins: Seq[In], outs: Set[Seq[FstOut]]): Segment[Out, Unit]

  def pipe[F[_]]: Pipe[F, In, Out] = {

    val step = Pull.loop[F, Out, (Stream[F, In], State, Seq[In], Seq[FstOut], Option[(Seq[In], Set[Seq[FstOut]])])] {
      case (s, state, accIn, accOut, lastMatch) =>
        s.pull.uncons1.flatMap {
          case Some((hd, tl)) =>
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
            find(matcher.extract(hd)) match {
              case Some((sym, tgt)) =>
                val accIn1 = accIn :+ hd
                val accOut1 = accOut ++ pfst.output(state, sym)
                val lastMatch1 =
                  if (pfst.isFinal(tgt)) {
                    // concatenate all final outputs to the accumulated outputs
                    val finalOutputs = pfst.finalOutput(tgt)
                    if (finalOutputs.isEmpty)
                      Some(accIn1 -> Set(accOut1))
                    else
                      Some(accIn1 -> finalOutputs.map(accOut1 ++ _))
                  } else {
                    lastMatch
                  }
                // continue matching
                Pull.pure(Some((tl, tgt, accIn1, accOut1, lastMatch1)))
              case None =>
                // does not match
                lastMatch match {
                  case Some((ins, outs)) =>
                    // emit and reinitializes state
                    val suffix = accIn.drop(ins.size)
                    Pull.output(reduce(ins, outs)).as(Some(makeContext(Stream(suffix: _*) ++ s)))
                  case None =>
                    // emit the passed-through head and reinitializes state
                    accIn match {
                      case Seq() =>
                        Pull.output(passThrough(hd)).as(Some(makeContext(tl)))
                      case Seq(fst, rest @ _*) =>
                        Pull.output(passThrough(fst)).as(Some(makeContext(Stream(rest: _*) ++ s)))
                    }
                }
            }
          case None =>
            lastMatch match {
              case Some((ins, outs)) =>
                // emit and reinitializes state
                val suffix = accIn.drop(ins.size)
                Pull.output(reduce(ins, outs)).as(Some(makeContext(Stream(suffix: _*))))
              case None =>
                accIn match {
                  case Seq() =>
                    // pull is done
                    Pull.pure(None)
                  case Seq(fst, rest @ _*) =>
                    Pull.output(passThrough(fst)).as(Some(makeContext(Stream(rest: _*))))
                }
            }
        }
    }

    in => step(makeContext(in)).stream
  }

  private def makeContext[F[_]](s: Stream[F, In]) = (s, pfst.initial, Seq.empty, Seq.empty, None)

}
