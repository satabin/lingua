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

import better.files.File

sealed abstract class DikoOptions extends Options {

  def mkCompile: CompileOptions =
    CompileOptions(verbose = verbose, timing = timing)

  def mkQuery: QueryOptions =
    QueryOptions(verbose = verbose, timing = timing)

  def mkVerbose: DikoOptions
  def mkTimed: DikoOptions

}

final case class NoCommandOptions(verbose: Boolean = false,
    timing: Boolean = false) extends DikoOptions {

  def mkVerbose: NoCommandOptions =
    copy(verbose = true)

  def mkTimed: NoCommandOptions =
    copy(timing = true)

}

final case class CompileOptions(input: File = null,
    generateInflections: Boolean = true,
    generateDeflections: Boolean = false,
    occupation: Int = 70,
    output: File = File("dikput.diko"),
    saveNFst: Option[File] = None,
    saveFst: Option[File] = None,
    verbose: Boolean = false,
    timing: Boolean = false) extends DikoOptions {

  def mkVerbose: CompileOptions =
    copy(verbose = true)

  def mkTimed: CompileOptions =
    copy(timing = true)

}

final case class QueryOptions(input: File = null,
    query: String = null,
    verbose: Boolean = false,
    timing: Boolean = false) extends DikoOptions {

  def mkVerbose: QueryOptions =
    copy(verbose = true)

  def mkTimed: QueryOptions =
    copy(timing = true)

}
