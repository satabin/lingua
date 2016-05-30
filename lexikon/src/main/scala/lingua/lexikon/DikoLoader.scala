/* Copyright (c) 2015 Lucas Satabin
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
package phases

import compiled._

import better.files._

import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption

import scodec.bits._
import scodec.Attempt

class DikoLoader extends Phase[QueryOptions, CompiledPSubFst](Some("diko-loader")) {

  val empty = CompiledPSubFst(Vector.empty, Vector.empty, ByteVector.empty, Vector.empty, Vector.empty)

  def process(options: QueryOptions, reporter: Reporter): CompiledPSubFst = {
    var channel: FileChannel = null
    try {
      channel = options.input.newFileChannel(Seq(StandardOpenOption.READ))
      FstProtocol.file.decodeValue(BitVector.fromChannel(channel)) match {
        case Attempt.Successful(fst) =>
          fst
        case Attempt.Failure(err) =>
          reporter.error(err.toString)
          empty
      }
    } catch {
      case e: Exception =>
        reporter.error(f"Error while loading ${options.input}", exn = Some(e))
        empty
    } finally {
      if (channel != null) {
        channel.close
      }
    }
  }

}
