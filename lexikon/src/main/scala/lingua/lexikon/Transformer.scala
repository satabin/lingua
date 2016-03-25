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

import lingua.parser.TagEmission
import parser._
import fst._

import scala.annotation.tailrec

import scala.util.matching.Regex

sealed trait Out
final case class CharOut(c: Char) extends Out
final case class CatOut(c: String) extends Out
final case class TagOut(present: Boolean, t: String) extends Out

class Transformer(reporter: Reporter, diko: Diko) {

  private val fstBuilder = Builder.create[Char, Out]

  private var fst: NFst[Option[Char], Out] = null

  // a regular expression that matches a non empty sequence of letters from the alphabet
  private val lettersRe =
    f"[${Regex.quote(diko.alphabet.mkString)}]+"

  import Builder._

  def transform(): NFst[Option[Char], Out] = {
    // assume everything type-checks
    if (fst == null) {
      for (l @ Lexikon(name, gCat, gTags, entries) <- diko.lexika) {
        val (words, rewrites) =
          entries.foldLeft(List.empty[Word], List.empty[Rewrite]) {
            case (acc, w @ Word(_, Some(_), _)) if gCat.isDefined =>
              reporter.error(w.offset, f"A global category is already defined on the lexikon")
              acc
            case (acc, w @ Word(_, None, _)) if !gCat.isDefined =>
              reporter.error(w.offset, f"A category must be defined for the word")
              acc
            case ((ws, rs), w @ Word(str, eCat, eTags)) =>
              (Word(str, gCat.orElse(eCat), gTags ++ eTags)(w.offset) :: ws, rs)
            case ((ws, rs), r @ Rewrite(name, eTags, rules)) =>
              (ws, Rewrite(name, gTags ++ eTags, rules)(r.offset) :: rs)
          }

        for (Word(inChars, cat, tags) <- words) {
          val start = fstBuilder.newState.makeInitial
          createStates(inChars, inChars, cat.get, tags, 0, start)
        }

        for (Rewrite(name, tags, rules) <- rewrites) {
          // a rewrite rule applies all its patterns in order to the words in this
          // lexicon (collected aboved). The first pattern that applies to a word
          // is the one taken, and the replacement is substituted to the word
          val rewrittenWords = rewriteWords(words, tags, rules)
          for ((original, Word(inChars, cat, tags)) <- rewrittenWords) {
            val start = fstBuilder.newState.makeInitial
            createStates(original, inChars, cat.get, tags, 0, start)
          }
        }
      }
      fst = fstBuilder.build()
    }
    fst
  }

  @tailrec
  private def createStates(inChars: String, outChars: String, cat: String, tags: Seq[TagEmission], idx: Int, previous: StateBuilder[Char, Out]): Unit = {
    assert(inChars.size == outChars.size, "")
    if (idx == inChars.size) {
      // final state
      val tags1 =
        tags.map {
          case (pres, t) =>
            TagOut(pres, t)
        }
      previous.makeFinal.addOutput(tags1 :+ CatOut(cat))
    } else {
      // add transition with the current character to a new state
      val st = fstBuilder.newState
      val inChar =
        if (inChars(idx) == '\u0000')
          None
        else
          Some(inChars(idx))
      val outChar =
        if (outChars(idx) == '\u0000')
          Seq()
        else
          Seq(CharOut(outChars(idx)))
      previous.addTransition(inChar, outChar, st)
      createStates(inChars, outChars, cat, tags, idx + 1, st)
    }
  }

  private def rewriteWords(words: List[Word], rTags: Seq[TagEmission], rules: Seq[Rule]): List[(String, Word)] = {
    def applyRewrite(pattern: Pattern, replacement: Replacement): List[(String, Word)] = {
      val Pattern(affix, seq, category, tags) = pattern
      val compiledPattern = compilePattern(affix, seq)
      for {
        word @ Word(_, wCategory, wTags) <- words
        if category == wCategory && tags.forall(wTags.contains(_))
        res <- rewriteWord(word, compiledPattern, rTags, replacement)
      } yield res
    }
    ???
  }

  private def rewriteWord(original: Word, pattern: Regex, rTags: Seq[TagEmission], replacement: Replacement): Option[(String, Word)] =
    pattern.findFirstMatchIn(original.word) match {
      case Some(m) =>
        val (startIdx, endIdx) = replacement.affix match {
          case Prefix =>
            (0, -1)
          case Suffix =>
            (-1, original.word.size)
          case Infix =>
            (0, original.word.size)
          case NoAffix =>
            (-1, -1)
        }
        ???
      case None =>
        None
    }

  private def compilePattern(affix: Affix, pattern: Seq[CasePattern]): Regex = {
    val compiledPattern =
      pattern.foldLeft(new StringBuilder) {
        case (acc, StringPattern(s)) =>
          acc.append(Regex.quote(s))
        case (acc, CapturePattern(n)) =>
          acc.append(f"($lettersRe)")
        //case (acc, EmptyPattern) =>
        //  acc.append("$")
      }
    affix match {
      case Prefix =>
        new Regex("^" + compiledPattern)
      case Suffix =>
        new Regex(compiledPattern.append("$").toString)
      case Infix =>
        new Regex(compiledPattern.toString)
      case NoAffix =>
        new Regex("^" + compiledPattern + "$")
    }
  }

}
