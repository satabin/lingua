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

import parser._
import fst._

import scala.annotation.tailrec

sealed trait Out
final case class CharOut(c: Char) extends Out
final case class CatOut(c: String) extends Out
final case class TagOut(present: Boolean, t: String) extends Out

class Transformer(reporter: Reporter, diko: Diko) {

  private val fstBuilder = Builder.create[Char, Out]

  private var fst: NFst[Option[Char], Out] = null

  import Builder._

  def transform(): NFst[Option[Char], Out] = {
    // assume everything type-checks
    if (fst == null) {
      for {
        l @ Lexikon(name, gCat, gTags, entries) <- diko.lexika
        entry <- entries
      } entry match {
        case w @ Word(_, Some(_), _) if gCat.isDefined =>
          reporter.error(w.offset, f"A global category is already defined on the lexikon")
        case w @ Word(_, None, _) if !gCat.isDefined =>
          reporter.error(w.offset, f"A category must be defined for the word")
        case w @ Word(inChars, eCat, eTags) =>
          @tailrec
          def createStates(idx: Int, previous: StateBuilder[Char, Out]): Unit =
            if (idx == inChars.size) {
              // final state
              val cat = gCat.orElse(eCat).get
              val tags =
                (gTags ++ eTags).foldLeft(Seq.empty[Out]) {
                  case (acc, (pres, t)) =>
                    acc :+ TagOut(pres, t)
                }
              previous.makeFinal.addOutput(tags :+ CatOut(cat))
            } else {
              // add transition with the current character to a new state
              val st = fstBuilder.newState
              previous.addTransition(Some(inChars(idx)), Seq(CharOut(inChars(idx))), st)
              createStates(idx + 1, st)
            }
          val start = fstBuilder.newState.makeInitial
          createStates(0, start)
        case r @ Rewrite(name, eTags, rules) =>

      }
      fst = fstBuilder.build()
    }
    fst
  }

}
