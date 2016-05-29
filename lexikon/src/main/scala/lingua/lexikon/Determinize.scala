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
package lexikon

import fst._

import scala.io.Codec

class Determinize(nfst: NFst[Char, Out]) extends Phase[PSubFst[Char, Out]] {

  def process(options: Options, reporter: Reporter): PSubFst[Char, Out] = {

    for (f <- options.saveNFst)
      f.overwrite(nfst.toDot)(codec = Codec.UTF8)

    val fst = nfst.determinize

    for (f <- options.saveFst)
      f.overwrite(fst.toDot)(codec = Codec.UTF8)

    fst

  }

}
