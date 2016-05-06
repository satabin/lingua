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

import fastparse.core.Parsed

import parser._

import java.io.File

import scala.io.Source

object Dikoc extends App {

  val optParser = new scopt.OptionParser[Options]("dikoc") {
    head("dikoc", BuildInfo.version)
    opt[Unit]('V', "verbose").action { (_, c) =>
      c.copy(verbose = true)
    }.text("Turn on verbose mode")
    help("help").abbr("h").text("Print this usage text")
    version("version").abbr("v").text("Print the version")
    arg[File]("<file>").action { (f, c) =>
      c.copy(input = f)
    }.text("Input diko file to compile")
  }

  optParser.parse(args, Options()) match {
    case Some(options) =>

      val input = Source.fromFile(options.input).mkString

      val reporter = new ConsoleReporter(input)

      // do stuff
      DikoParser.diko.parse(input) match {
        case Parsed.Success(diko, _) =>
          val typer = new Typer(reporter, diko)

          typer.typeCheck()

          val transformer = new Transformer(reporter, diko)

          val nfst = transformer.transform();

          println(nfst.removeEpsilonTransitions.determinize.toDot)

        case f @ Parsed.Failure(_, offset, _) =>
          reporter.error(offset, f"Parse error")
      }

    case None =>
    // arguments are bad, error message will have been displayed
  }

}
