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

class ConsoleReporter(options: Options, input: String) extends Reporter(options, input) {

  import Reporter._

  protected def doReport(offset: Int, level: Level.Value, msg: String, exn: Option[Throwable]): Unit = {
    val pos = if (offset >= 0) {
      val (line, col) = lineColOf(offset)
      f"[$line,$col] "
    } else {
      ""
    }
    val severity =
      level match {
        case Level.VERBOSE => f"[${Console.GREEN}verbose${Console.RESET}]"
        case Level.INFO    => f"[${Console.BLUE}info${Console.RESET}]"
        case Level.WARNING => f"[${Console.YELLOW}warning${Console.RESET}]"
        case Level.ERROR   => f"[${Console.RED}error${Console.RESET}]"
      }
    println(f"$severity $pos$msg")
    for (e <- exn)
      e.printStackTrace
  }

}
