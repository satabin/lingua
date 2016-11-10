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
package lingua.lexikon
package compiled
package fst

import scodec.bits._
import scodec._
import scodec.codecs._

/** A codec for compiled compacted Fst.
 *
 *  @author Lucas Satabin
 */
object FstProtocol {

  object noop extends Codec[Unit] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = Attempt.Successful(DecodeResult((), bits))
    override def encode(u: Unit): Attempt[BitVector] = Attempt.Successful(BitVector.empty)
    override def sizeBound = SizeBound.unknown
  }

  val char: Codec[Char] =
    utf8_32.xmap[Char](_(0), _.toString)

  val output: Codec[Out] =
    "output" | discriminated[Out].by(byte)
      .|(0) { case CharOut(c) => c }(CharOut)(char)
      .|(1) { case CatOut(s) => s }(CatOut)(utf8_32)
      .|(2) { case TagOut(t) => t }(TagOut)(utf8_32)

  val outputs: Codec[Set[Seq[Int]]] =
    "outputs" | vectorOfN(int32, listOfN(int32, int32).xmap[Seq[Int]](_.toSeq, _.toList)).xmap[Set[Seq[Int]]](_.toSet, _.toVector)

  private val sizeBytes: Codec[(Int, ByteVector)] = int32.flatZip(bytes(_))

  val transitionIndexArray: Codec[ByteVector] =
    sizeBytes.xmap[ByteVector](_._2, bv => (bv.size.toInt, bv))

  val header: Codec[Unit] =
    "header" |
      ("magic_number" | constant(hex"D1C0F17E")) ~>
      ("version" | constant(0x01))

  val file: Codec[CompiledFst] =
    header ~> ("fst_type" |
      discriminated[CompiledFst].by(byte)
      .|(0) { case fst @ CompiledPSubFst(_, _, _, _, _) => fst }(identity)(psubfst.fst)
      .|(1) { case fst @ CompiledQPFst(_, _, _, _, _) => fst }(identity)(qpfst.fst))

  /** The protocol parts specific to [[CompiledQPFst]]. */
  object qpfst {

    val qpoutput: Codec[QPOutput] =
      "qpoutput" | discriminated[QPOutput].by(byte)
        .|(0) { case QPOut(c) => c }(QPOut)(int32)
        .|(1) { case QPIdentity => () }(_ => QPIdentity)(noop)
        .|(2) { case QPPop => () }(_ => QPPop)(noop)

    val qpoutputs: Codec[Set[Seq[QPOutput]]] =
      "outputs" | vectorOfN(int32, listOfN(int32, qpoutput).xmap[Seq[QPOutput]](_.toSeq, _.toList)).xmap[Set[Seq[QPOutput]]](_.toSet, _.toVector)

    val transition: Codec[QPTransition] =
      ("transition" |
        ("in" | char) ::
        ("capture" | bool) ::
        ("out" | listOfN(int32, qpoutput)) ::
        ("target" | int32)).as[QPTransition]

    val fst: Codec[CompiledQPFst] =
      ("fst" |
        ("alphabet" | vectorOfN(int32, char)) ::
        ("outputs" | vectorOfN(int32, output)) ::
        ("transition_index_array" | transitionIndexArray) ::
        ("transition_array" | vectorOfN(int32, transition)) ::
        ("outputs_array" | vectorOfN(int32, qpoutputs))).as[CompiledQPFst]

  }

  /** The protocol parts specific to [[CompiledPSubFst]]. */
  object psubfst {

    val transition: Codec[PSubTransition] =
      ("transition" |
        ("in" | char) ::
        ("out" | listOfN(int32, int32)) ::
        ("target" | int32)).as[PSubTransition]

    val fst: Codec[CompiledPSubFst] =
      ("fst" |
        ("alphabet" | vectorOfN(int32, char)) ::
        ("outputs" | vectorOfN(int32, output)) ::
        ("transition_index_array" | transitionIndexArray) ::
        ("transition_array" | vectorOfN(int32, transition)) ::
        ("outputs_array" | vectorOfN(int32, outputs))).as[CompiledPSubFst]

  }

}
