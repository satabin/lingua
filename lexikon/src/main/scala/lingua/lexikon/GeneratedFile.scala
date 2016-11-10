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

import fst.{
  PSubFst,
  QPFst
}

sealed trait GeneratedFile

final case class PSubFstFile(file: File, fst: PSubFst[Char, Out]) extends GeneratedFile

final case class QPFstFile(file: File, fst: QPFst[Char, Out]) extends GeneratedFile

final case class DotFile(file: File, dot: String) extends GeneratedFile
