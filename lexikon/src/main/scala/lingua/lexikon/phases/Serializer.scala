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
package phases

import compiled._

import scodec.Attempt

import better.files._

class Serializer(compiled: CompiledPSubFst) extends Phase[Unit](Some("serializer")) {

  def process(options: Options, reporter: Reporter): Unit = {
    FstProtocol.fst.encode(compiled) match {
      case Attempt.Successful(bytes) =>
        for (raf <- options.output.newRandomAccess(File.RandomAccessMode.readWrite).autoClosed)
          raf.write(bytes.toByteArray)
      case Attempt.Failure(err) =>
        reporter.error(err.toString)
    }
  }

}
