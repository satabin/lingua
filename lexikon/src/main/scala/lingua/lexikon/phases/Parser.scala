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
package phases

import parser._
import untyped._

import fastparse.core.Parsed

class Parser(inputs: Map[String, String]) extends Phase[CompileOptions, Seq[DikoUnit]](Some("parser")) {

  def process(options: CompileOptions, reporter: Reporter): Seq[DikoUnit] =
    (for {
      (name, input) <- inputs
      unit <- DikoParser.unit(name).parse(input) match {
        case Parsed.Success(unit, _) =>
          Some(unit)
        case failure @ Parsed.Failure(_, offset, extra) =>
          reporter.error(f"Unexpected input. Expected: ${extra.traced.expected}", name, offset)
          None
      }
    } yield unit).toSeq

}
