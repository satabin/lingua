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

object DikoMain extends App {

  implicit val fileRead: Read[File] = Read.reads(File(_))

  val optParser = new OptionParser[DikoOptions]("diko") {
    head("diko", BuildInfo.version)

    cmd("compile").action { (_, c) =>
      c.mkCompile
    }.text("Compile a dictionary").children(
      opt[File]('o', "output-dir").action {
        case (f, c: CompileOptions) =>
          c.copy(outputDir = f)
        case _ =>
          throw new Exception
      },
      opt[String]("inflections-file").action {
        case (f, c: CompileOptions) =>
          c.copy(inflectionsFile = f)
        case _ =>
          throw new Exception
      }.text("The generated inflections files base name (by default 'inflections')"),
      opt[String]("deflexions-file").action {
        case (f, c: CompileOptions) =>
          c.copy(deflexionsFile = f)
        case _ =>
          throw new Exception
      }.text("The generated deflexions files base name (by default 'deflexions')"),
      opt[String]("lemmas-file").action {
        case (f, c: CompileOptions) =>
          c.copy(lemmasFile = f)
        case _ =>
          throw new Exception
      }.text("The generated lemmas files base name (by default 'lemmas')"),
      opt[Unit]('i', "inflections").action {
        case (b, c: CompileOptions) =>
          c.copy(generateInflections = true)
        case _ =>
          throw new Exception
      }.text("Generate inflections from the lemmas and rewrite rules"),
      opt[Unit]('d', "deflections").action {
        case (b, c: CompileOptions) =>
          c.copy(generateDeflexions = true)
        case _ =>
          throw new Exception
      }.text("Generate deflexions from the rewrite rules"),
      opt[Unit]('l', "lemmas").action {
        case (b, c: CompileOptions) =>
          c.copy(generateLemmas = true)
        case _ =>
          throw new Exception
      }.text("Generate lemmas"),
      opt[Int]('K', "occupation").action {
        case (k, c: CompileOptions) =>
          c.copy(occupation = k)
        case _ =>
          throw new Exception
      }.text("Minimum segement occupation before skipping to the next one. Decreasing this number improves generation speed but results in bigger generated lexicon file.")
        .validate(i => if (i >= 0 && i <= 100) success else failure("value must be between 0 and 100")),
      opt[Unit]('N', "save-nfst").action {
        case (f, c: CompileOptions) =>
          c.copy(saveNFst = true)
        case _ =>
          throw new Exception
      }.text("Save the dot representation of the non deterministic fst"),
      opt[Unit]('F', "save-fst").action {
        case (f, c: CompileOptions) =>
          c.copy(saveFst = true)
        case _ =>
          throw new Exception
      }.text("Save the dot representation of the fst"),
      arg[File]("<file>").action {
        case (f, c: CompileOptions) =>
          c.copy(input = f)
        case _ =>
          throw new Exception
      }.text("Input diko file to compile").required())

    note("")
    cmd("query").action { (_, c) =>
      c.mkQuery
    }.text("Query a dictionary").children(
      opt[String]('q', "query").action {
        case (q, c: QueryOptions) =>
          c.copy(query = q)
        case _ =>
          throw new Exception
      }.text("The word to query in the compiled diko file").required(),
      arg[File]("<file>").action {
        case (f, c: QueryOptions) =>
          c.copy(input = f)
        case _ =>
          throw new Exception
      }.text("Compiled diko file to query").required())

    note("")
    note("Global options")
    opt[Unit]('t', "timing").action { (_, c) =>
      c.mkTimed
    }.text("Turn on phase timing")

    opt[Unit]('V', "verbose").action { (_, c) =>
      c.mkVerbose
    }.text("Turn on verbose mode")

    help("help").abbr("h").text("Print this usage text")
    version("version").abbr("v").text("Print the version")
  }

  for (options <- optParser.parse(args, NoCommandOptions())) {

    options match {
      case options: CompileOptions =>
        val input = options.input.contentAsString(codec = Codec.UTF8)
        val reporter = new ConsoleReporter(options, input)

        // do stuff
        val sequence =
          for {
            diko <- new Parser(input)
            typer <- new Typer(diko)
            files <- new Transformer(typer, diko)
            () <- new Serializer(files, diko)
          } yield ()

        sequence.run(options, reporter)
        reporter.info(f"Files written in ${options.outputDir}")
        reporter.summary()

      case options: QueryOptions =>
        val reporter = new ConsoleReporter(options, "")
        val sequence =
          for {
            fst <- new DikoLoader()
            result <- new Lookup(fst)
          } yield result

        println(sequence.run(options, reporter))

      case _ =>
        println("Command is required")
        optParser.showUsageAsError()

    }

  }

}
