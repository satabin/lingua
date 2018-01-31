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
package cascade

/** A layer is a basic part of a cascade.
 *  When data is reduced or passed-through, one may use and modify
 *  a context.
 *  This can be used, e.g., to assign the current position in the stream.
 *
 */
abstract class Layer[In, Out, Ctx] extends Transformer[In, Out] {

  /** For each unmatched input element, how it is
   *  passed through to the output stream.
   */
  protected def passThrough(ctx: Ctx, in: In): (Ctx, Out)

  /** For each matched sequence of input, reduce to the produced
   *  out value.
   *  If the matching values must be dropped then, return `None`.
   */
  protected def reduce(ctx: Ctx, ins: Seq[In]): (Ctx, Option[Out])

  /** Process one step of this layer on the current stream.
   *  If the head can be processed by this layer, then
   *  returns the matched sequence and the rest stream,
   *  otherwise, returns `None`.
   */
  protected def one(s: Stream[In]): Option[(Seq[In], Stream[In])]

  /** Creates a new context for this layer.
   *  This is called whenever transformation of a stream begins.
   *  If the context is not immutable, make sure this methode returns
   *  a new instance for each call, otherwise concurrent use of the
   *  layer may lead to inconsistencies.
   */
  def makeContext: Ctx

  final def transform(s: Stream[In]): Stream[Out] = {
    val ctx = makeContext
    def loop(ctx: Ctx, s: Stream[In]): Stream[Out] =
      one(s) match {
        case Some((seq, rest)) =>
          // this layer matched the head of the current stream
          // reduce it and process further
          reduce(ctx, seq) match {
            case (ctx, Some(out)) => out #:: loop(ctx, rest)
            case (ctx, None)      => loop(ctx, rest)
          }
        case None =>
          // this layer did not match the head of the current stream
          // pass through the first element and process further
          s match {
            case h #:: rest =>
              val (ctx1, o) = passThrough(ctx, h)
              o #:: loop(ctx1, rest)
            case _ => Stream.Empty
          }
      }
    loop(ctx, s)
  }

  final def andThen[Out1](that: Transformer[Out, Out1]): Transformer[In, Out1] =
    new Cascade(this, that)

}
