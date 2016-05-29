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

import scopt._

import parser._
import compiled._
import phases._

import better.files._

import scala.io.Codec

import scodec.bits._
import scodec.Attempt

object Dikoc extends App {

  implicit val fileRead: Read[File] = Read.reads(File(_))

  val optParser = new OptionParser[Options]("dikoc") {
    head("dikoc", BuildInfo.version)
    opt[File]('o', "output").action { (f, c) =>
      c.copy(output = f)
    }.text("The output .diko file (by default 'dikoput.diko'")
    opt[String]('q', "query").action { (s, c) =>
      c.copy(query = Some(s))
    }.text("The word to query if reading a compiled diko file")
    opt[Unit]('t', "timing").action { (_, c) =>
      c.copy(timing = true)
    }.text("Turn on phase timing")
    opt[Unit]('V', "verbose").action { (_, c) =>
      c.copy(verbose = true)
    }.text("Turn on verbose mode")
    opt[File]('N', "save-nfst").action { (f, c) =>
      c.copy(saveNFst = Some(f))
    }.text("Save the dot representation of the non deterministic fst to the given file")
    opt[File]('F', "save-fst").action { (f, c) =>
      c.copy(saveFst = Some(f))
    }.text("Save the dot representation of the fst to the given file")
    help("help").abbr("h").text("Print this usage text")
    version("version").abbr("v").text("Print the version")
    arg[File]("<file>").action { (f, c) =>
      c.copy(input = f)
    }.text("Input diko file to compile")
  }

  for (options <- optParser.parse(args, Options())) {

    val head = BitVector(options.input.bytes.take(5))
    FstProtocol.header.decodeValue(head) match {
      case Attempt.Successful(()) =>
        val reporter = new ConsoleReporter("")
        val fst = new DikoLoader().process(options, reporter)

        println(fst.lookup(options.query.get))

      case Attempt.Failure(_) =>
        val input = options.input.contentAsString(codec = Codec.UTF8)
        val reporter = new ConsoleReporter(input)

        // do stuff
        val sequence =
          for {
            diko <- new Parser(input)
            typer <- new Typer(diko)
            nfst <- new Transformer(typer, diko)
            fst <- new Determinize(nfst)
            compiled <- new Compiler(fst, diko)
            () <- new Serializer(compiled)
          } yield ()

        sequence.run(options, reporter)
        reporter.info(f"output wirtten in ${options.output}")
        reporter.summary()

    }

  }

}
