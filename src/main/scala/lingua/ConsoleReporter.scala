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

object ConsoleReporter extends Reporter {

  import Reporter._

  protected def doReport(level: Level.Value, msg: String, exn: Option[Exception]): Unit = {
    val severity =
      level match {
        case Level.INFO    => "[info]"
        case Level.WARNING => f"[${Console.YELLOW}warning${Console.RESET}]"
        case Level.ERROR   => f"[${Console.RED}error${Console.RESET}]"
      }
    println(f"$severity $msg")
  }

}
