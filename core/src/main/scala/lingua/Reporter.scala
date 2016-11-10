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

import scala.annotation.tailrec

import scala.collection.mutable.ArrayBuffer

object Reporter {
  object Level extends Enumeration {
    val VERBOSE, INFO, WARNING, ERROR = Value
  }
}

abstract class Reporter(options: Options, inputs: Map[String, String]) {

  import Reporter._

  protected def doReport(name: String, offset: Int, level: Level.Value, msg: String, exn: Option[Throwable]): Unit

  def report(name: String, offset: Int, level: Level.Value, msg: String, exn: Option[Throwable]): Unit = {
    if (level == Level.ERROR)
      errors += 1
    else if (level == Level.WARNING)
      warnings += 1
    if (options.verbose || level != Level.VERBOSE)
      doReport(name, offset, level, msg, exn)
  }

  def error(msg: String, name: String = "", offset: Int = -1, exn: Option[Throwable] = None): Unit =
    report(name, offset, Level.ERROR, msg, exn)

  def warning(msg: String, name: String = "", offset: Int = -1, exn: Option[Throwable] = None): Unit =
    report(name, offset, Level.WARNING, msg, exn)

  def info(msg: String, name: String = "", offset: Int = -1, exn: Option[Throwable] = None): Unit =
    report(name, offset, Level.INFO, msg, exn)

  def verbose(msg: String, name: String = "", offset: Int = -1): Unit =
    report(name, offset, Level.VERBOSE, msg, None)

  def summary(): Unit = {
    doReport("", -1, Level.INFO, f"number of warnings: $warnings", None)
    doReport("", -1, Level.INFO, f"number of errors: $errors", None)
  }

  private var errors = 0
  private var warnings = 0

  def hasErrors: Boolean =
    errors > 0

  private val lines =
    for ((name, input) <- inputs) yield {
      val lines = """\r?\n""".r.findAllMatchIn(input).foldLeft(ArrayBuffer(0)) {
        case (acc, m) =>
          acc.append(m.end)
          acc
      }
      (name, lines.result)
    }

  protected def lineColOf(name: String, offset: Int): (Int, Int) = {
    val lines = this.lines(name)
    @tailrec
    def start(idx: Int): Int =
      if (offset < lines(idx))
        start(idx - 1)
      else
        idx
    if (offset < 0) {
      (-1, -1)
    } else {
      val sidx = start(lines.size - 1)
      (sidx + 1, offset - lines(sidx))
    }
  }

}
