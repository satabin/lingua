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

import fastparse.core.Parsed

import parser._
import compiled._

import scala.io.Codec

import better.files._

import scodec.Attempt

object Dikoc extends App {

  implicit val fileRead: Read[File] = Read.reads(File(_))

  val optParser = new OptionParser[Options]("dikoc") {
    head("dikoc", BuildInfo.version)
    opt[File]('o', "output").action { (f, c) =>
      c.copy(output = f)
    }
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

    val input = options.input.contentAsString(codec = Codec.UTF8)

    val reporter = new ConsoleReporter(input)

    // do stuff
    DikoParser.diko.parse(input) match {
      case Parsed.Success(diko, _) =>
        val typer = new Typer(reporter, diko)

        typer.typeCheck()

        if (reporter.hasErrors)
          sys.exit(1)

        val transformer = new Transformer(typer, reporter, diko)

        val nfst = transformer.transform().removeEpsilonTransitions

        if (reporter.hasErrors)
          sys.exit(1)

        for (f <- options.saveNFst)
          f.overwrite(nfst.toDot)(codec = Codec.UTF8)

        val fst = nfst.determinize

        for (f <- options.saveFst)
          f.overwrite(fst.toDot)(codec = Codec.UTF8)

        val compiler = new Compiler(reporter, fst, diko)

        val compiled = compiler.compile()

        FstProtocol.fst.encode(compiled) match {
          case Attempt.Successful(bytes) =>
            for (raf <- options.output.newRandomAccess(File.RandomAccessMode.readWrite).autoClosed)
              raf.write(bytes.toByteArray)
          case Attempt.Failure(err) =>
            reporter.error(err.toString)
        }

        if (reporter.hasErrors)
          sys.exit(1)

      case f @ Parsed.Failure(_, offset, _) =>
        reporter.error(f"Parse error", offset)
    }

  }

}
