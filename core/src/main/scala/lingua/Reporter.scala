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
    val INFO, WARNING, ERROR = Value
  }
}

abstract class Reporter(input: String) {

  import Reporter._

  protected def doReport(offset: Int, level: Level.Value, msg: String, exn: Option[Exception]): Unit

  def report(offset: Int, level: Level.Value, msg: String, exn: Option[Exception]): Unit = {
    if (level == Level.ERROR)
      errors += 1
    else if (level == Level.WARNING)
      warnings += 1
    doReport(offset, level, msg, exn)
  }

  def error(offset: Int, msg: String, exn: Option[Exception] = None): Unit =
    report(offset, Level.ERROR, msg, exn)

  def warning(offset: Int, msg: String, exn: Option[Exception] = None): Unit =
    report(offset, Level.ERROR, msg, exn)

  def info(offset: Int, msg: String, exn: Option[Exception] = None): Unit =
    report(offset, Level.ERROR, msg, exn)

  private var errors = 0
  private var warnings = 0

  def hasErrors: Boolean =
    errors > 0

  private val lines = ArrayBuffer(0)

  for (m <- """\r?\n""".r.findAllMatchIn(input))
    lines.append(m.end)

  protected def lineColOf(offset: Int): (Int, Int) = {
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
