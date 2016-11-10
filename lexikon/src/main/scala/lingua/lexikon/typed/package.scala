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

/** This package contains the typed and resolved elements that are handled
 *  after the typing phase.
 */
package object typed {

  type TagEmission = (Boolean, Tag)

  implicit class TagEmissionsOps(val tags: Seq[TagEmission]) extends AnyVal {

    def normalized: Seq[TagEmission] =
      Seq.empty[TagEmission].normalizeWith(tags)

    def normalizeWith(tags1: Seq[TagEmission]): Seq[TagEmission] =
      tags1.foldLeft(if (tags.isEmpty) tags else tags.normalized) {
        case (acc, tage) if acc.contains(tage) =>
          acc
        case (acc, (true, tag)) if acc.contains(false -> tag) =>
          acc.filterNot(_ == (false -> tag))
        case (acc, (false, tag)) if acc.contains(true -> tag) =>
          acc.filterNot(_ == (true -> tag))
        case (acc, tage) =>
          acc :+ tage
      }

  }

}
