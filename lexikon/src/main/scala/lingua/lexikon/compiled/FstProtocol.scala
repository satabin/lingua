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

import scodec.bits._
import scodec.Codec
import scodec.codecs._

/** A codec for compiled ocmpacted Fst.
 *
 *  @author Lucas Satabin
 */
object FstProtocol {

  val char: Codec[Char] =
    utf8_32.xmap[Char](_(0), _.toString)

  val output: Codec[Out] =
    "output" | discriminated[Out].by(byte)
      .|(0) { case CharOut(c) => c }(CharOut)(char)
      .|(1) { case CatOut(s) => s }(CatOut)(utf8_32)
      .|(2) { case TagOut(p, t) => (p, t) }(TagOut)(bool ~ utf8_32)

  val transition: Codec[Transition] =
    ("transition" |
      ("in" | char) ::
      ("out" | listOfN(int32, int32)) ::
      ("target" | int32) ::
      ("isFinal" | bool)).as[Transition]

  val fst: Codec[CompiledPSubFst] =
    ("fst" |
      ("alphabet" | vectorOfN(int32, char)) ::
      ("outputs" | vectorOfN(int32, output)) ::
      ("transition_index_array" | vectorOfN(int32, int64)) ::
      ("transition_array" | vectorOfN(int32, transition))).as[CompiledPSubFst]

  val header: Codec[Unit] =
    "header" |
      ("magic_number" | constant(hex"D1C0F17E")) ~>
      ("version" | constant(0x01))

  val file: Codec[CompiledPSubFst] =
    header ~> fst

}
