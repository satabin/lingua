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

object Reporter {
  object Level extends Enumeration {
    val INFO, WARNING, ERROR = Value
  }
}

abstract class Reporter {

  import Reporter._

  protected def doReport(level: Level.Value, msg: String, exn: Option[Exception]): Unit

  def report(level: Level.Value, msg: String, exn: Option[Exception]): Unit = {
    if (level == Level.ERROR)
      errors += 1
    else if (level == Level.WARNING)
      warnings += 1
    doReport(level, msg, exn)
  }

  def error(msg: String, exn: Option[Exception] = None): Unit =
    report(Level.ERROR, msg, exn)

  def warning(msg: String, exn: Option[Exception] = None): Unit =
    report(Level.ERROR, msg, exn)

  def info(msg: String, exn: Option[Exception] = None): Unit =
    report(Level.ERROR, msg, exn)

  private var errors = 0
  private var warnings = 0

  def nbOfErrors: Boolean =
    errors > 0

}
