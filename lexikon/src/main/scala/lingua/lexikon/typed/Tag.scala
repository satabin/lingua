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
package typed

sealed trait Tag {
  val alias: String
  val fullname: String
  val public: Boolean
  val concrete: Boolean

  val uname: String
  val offset: Int
}

/** A typed simple tag with its parent and visibility resolved. */
final case class ConcreteTag(alias: String, fullname: String, public: Boolean, parent: Option[AbstractTag])(val uname: String, val offset: Int) extends Tag {
  val concrete = true
}

/** A typed abstract tag. */
final case class AbstractTag(alias: String, fullname: String, public: Boolean)(val uname: String, val offset: Int) extends Tag {
  val concrete = false
}
