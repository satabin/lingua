/* Copyright (c) 2015 Lucas Satabin
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
package parser

class Lexical(keywords: Set[String]) {

  import fastparse.all._

  def keyword(s: String): P0 =
    P(s ~ !CharPred(c => c.isLetterOrDigit || c == '-')).opaque(s)

  val comment: P0 =
    NoTrace(P("//" ~ CharsWhile(_ != '\n', min = 0)))

  val wscomment: P0 =
    NoTrace(P((CharsWhile(" \n\t\f".toSet, min = 1) | comment).rep))

  val name: P[String] = P(
    CharPred(_.isLetterOrDigit).rep(min = 1, sep = "-".?).!.filter(!keywords.contains(_))
      | "`" ~/ (!"`" ~ AnyChar).rep(min = 0).! ~ "`").opaque("<name>")

  def alpha: P[String] =
    P("'" ~ name)

  object WsApi extends fastparse.WhitespaceApi.Wrapper(wscomment)

}
