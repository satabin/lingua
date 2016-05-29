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

import lingua.parser.TagEmission
import parser._
import fst._

import scala.annotation.tailrec

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

import gnieh.diff._

/** The transformer is the core class of the lexicon generator. It takes a well-typed lexicon
 *  description and produces a non-deterministic Fst with epsilon transitions.
 *
 *  @author Lucas Satabin
 */
class Transformer(typer: Typer, diko: Diko) extends Phase[NFst[Char, Out]](Some("transformer")) {

  private val lcs = new Patience[Char]

  private val fstBuilder = Builder.create[Char, Out]

  private var fst: NFst[Char, Out] = null

  // a regular expression that matches a non empty sequence of letters from the alphabet
  private val lettersRe =
    f"[${Regex.quote(diko.alphabet.mkString)}]+"

  import Builder._

  def process(options: Options, reporter: Reporter): NFst[Char, Out] = {
    // assume everything type-checks
    if (fst == null) {
      for (l @ Lexikon(name, gCat, gTags, entries) <- diko.lexika) {
        val (words, rewrites) =
          entries.foldLeft(List.empty[Word], List.empty[Rewrite]) {
            case (acc, w @ Word(_, Some(_), _)) if gCat.isDefined =>
              reporter.error(f"A global category is already defined on the lexikon", w.offset)
              acc
            case (acc, w @ Word(_, None, _)) if !gCat.isDefined =>
              reporter.error(f"A category must be defined for the word", w.offset)
              acc
            case ((ws, rs), w @ Word(str, eCat, eTags)) =>
              (Word(str, gCat.orElse(eCat), gTags ++ eTags)(w.offset) :: ws, rs)
            case ((ws, rs), r @ Rewrite(name, eTags, rules)) =>
              (ws, Rewrite(name, gTags ++ eTags, rules)(r.offset) :: rs)
          }

        for (Word(input, cat, tags) <- words) {
          val start = fstBuilder.newState.makeInitial
          val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
          createStates(inChars, outChars, cat.get, tags, 0, start)
        }

        for (Rewrite(name, tags, rules) <- rewrites) {
          // a rewrite rule applies all its patterns in order to the words in this
          // lexicon (collected aboved). The first pattern that applies to a word
          // is the one taken, and the replacement is substituted to the word
          val rewrittenWords = rewriteWords(words, tags, rules)
          for (Word(input, cat, tags) <- rewrittenWords) {
            val start = fstBuilder.newState.makeInitial
            val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
            createStates(inChars, outChars, cat.get, tags, 0, start)
          }
        }
      }
      fst = fstBuilder.build().removeEpsilonTransitions
    }
    fst
  }

  @tailrec
  private def createStates(inChars: Vector[Option[Char]], outChars: Vector[Option[Char]], cat: String, tags: Seq[TagEmission], idx: Int, previous: StateBuilder[Char, Out]): Unit = {
    assert(inChars.size == outChars.size, "")
    if (idx == inChars.size) {
      // final state
      val tags1 =
        normalizedTags(Seq.empty, tags).foldLeft(Seq.empty[TagOut]) {
          case (acc, (true, t)) if typer.isPublic(t) =>
            acc :+ TagOut(t)
          case (acc, _) =>
            acc
        }
      previous.makeFinal.addOutput(tags1 :+ CatOut(cat))
    } else {
      // add transition with the current character to a new state
      val st = fstBuilder.newState
      val outChar =
        outChars(idx) match {
          case Some(c) => Seq(CharOut(c))
          case None    => Seq()
        }
      previous.addTransition(inChars(idx), outChar, st)
      createStates(inChars, outChars, cat, tags, idx + 1, st)
    }
  }

  private def rewriteWords(words: List[Word], rTags: Seq[TagEmission], rules: Seq[Rule]): List[Word] =
    for {
      word <- words
      rule <- rules
      rewritten <- applyRule(word, rule, rTags, rule)
    } yield rewritten

  private def applyPattern(word: Word, rule: Rule, rTags: Seq[TagEmission], pattern: Pattern, replacement: Replacement): Option[Word] = {
    val Pattern(affix, seq, category, tags) = pattern
    val (mustTags, mustntTags) =
      tags.foldLeft((Set.empty[String], Set.empty[String])) {
        case ((mustTags, mustntTags), (true, tag))  => (mustTags + tag, mustntTags)
        case ((mustTags, mustntTags), (false, tag)) => (mustTags, mustntTags + tag)
      }

    val compiledPattern = compilePattern(affix, seq)
    rewriteWord(word, rule, compiledPattern, category, mustTags, mustntTags, rTags, replacement)
  }

  private def applyRule(word: Word, origin: Rule, rTags: Seq[TagEmission], rule: Rule): Option[Word] =
    if (rule.isEmpty) {
      None
    } else {
      val (pat, repl) = rule.head
      applyPattern(word, origin, rTags, pat, repl).orElse(applyRule(word, origin, rTags, rule.tail))
    }

  private def buildString(offset: Int, category: Option[String], rule: Rule, m: Match, seq: Seq[CaseReplacement], builder: StringBuilder): Unit =
    for (repl <- seq)
      repl match {
        case StringReplacement(s) =>
          builder.append(s)
        case CaptureReplacement(n) =>
          builder.append(m.group(n))
        case RecursiveReplacement(seq) =>
          val substring = new StringBuilder
          buildString(offset, category, rule, m, seq, substring)
          val newSeq = substring.toSeq.map(c => WordChar(Some(c), Some(c)))
          val subword = Word(newSeq, category, Seq.empty)(offset)
          applyRule(subword, rule, Seq.empty, rule) match {
            case Some(w) => builder.append(w.word)
            case None    => builder.append(subword.word)
          }
      }

  private def rewriteWord(original: Word, rule: Rule, pattern: Regex, mustCategory: Option[String], mustTags: Set[String], mustntTags: Set[String], rTags: Seq[TagEmission], replacement: Replacement): Option[Word] = {
    val normalized = normalizedTags(Seq.empty, original.tags)
    if ((mustCategory.isEmpty || mustCategory == original.category) && mustTags.forall(t => normalized.exists(tag => typer.isA(tag, t))) && mustntTags.forall(t => !normalized.exists(tag => typer.isA(tag, t)))) {
      for (m <- pattern.findFirstMatchIn(original.word)) yield {
        // build the new word based on original and replacement text
        val builder = new StringBuilder
        if (m.start > 0)
          builder.append(original.word.substring(0, m.start))
        buildString(original.offset, original.category, rule, m, replacement.seq, builder)
        if (m.end < original.word.size)
          builder.append(original.word.substring(m.end, original.word.size))
        val chars = buildWordChars(original.word, builder.toString)
        Word(chars, original.category, normalizedTags(normalized, rTags))(original.offset)
      }
    } else {
      None
    }
  }

  private def compilePattern(affix: Affix, pattern: Seq[CasePattern]): Regex = {
    val compiledPattern =
      pattern.foldLeft(new StringBuilder) {
        case (acc, StringPattern(s)) =>
          acc.append(Regex.quote(s))
        case (acc, CapturePattern(n)) =>
          acc.append(f"($lettersRe)")
      }
    affix match {
      case Prefix =>
        new Regex(f"^$compiledPattern")
      case Suffix =>
        new Regex(f"$compiledPattern$$")
      case Infix =>
        new Regex(compiledPattern.toString)
      case NoAffix =>
        new Regex(f"^$compiledPattern$$")
    }
  }

  private def buildWordChars(original: String, rewritten: String): Seq[WordChar] = {
    @tailrec
    def loop(rewrittenIdx: Int, originalIdx: Int, indices: List[(Int, Int)], acc: List[WordChar]): List[WordChar] = indices match {
      case (rIdx, oIdx) :: _ if rIdx > rewrittenIdx && oIdx > originalIdx =>
        loop(rewrittenIdx + 1, originalIdx + 1, indices, WordChar(Some(rewritten(rewrittenIdx)), Some(original(originalIdx))) :: acc)
      case (rIdx, _) :: _ if rIdx > rewrittenIdx =>
        loop(rewrittenIdx + 1, originalIdx, indices, WordChar(Some(rewritten(rewrittenIdx)), None) :: acc)
      case (_, oIdx) :: _ if oIdx > originalIdx =>
        loop(rewrittenIdx, originalIdx + 1, indices, WordChar(None, Some(original(originalIdx))) :: acc)
      case _ :: rest =>
        loop(rewrittenIdx + 1, originalIdx + 1, rest, WordChar(Some(rewritten(rewrittenIdx)), Some(original(originalIdx))) :: acc)
      case Nil if rewrittenIdx < rewritten.size && originalIdx < original.size =>
        loop(rewrittenIdx + 1, originalIdx + 1, indices, WordChar(Some(rewritten(rewrittenIdx)), Some(original(originalIdx))) :: acc)
      case Nil if rewrittenIdx < rewritten.size =>
        loop(rewrittenIdx + 1, originalIdx, indices, WordChar(Some(rewritten(rewrittenIdx)), None) :: acc)
      case Nil if originalIdx < original.size =>
        loop(rewrittenIdx, originalIdx + 1, indices, WordChar(None, Some(original(originalIdx))) :: acc)
      case Nil =>
        acc.reverse
    }
    val indices = lcs.lcs(rewritten, original)
    loop(0, 0, indices, Nil)
  }

  private def normalizedTags(tags1: Seq[TagEmission], tags2: Seq[TagEmission]): Seq[TagEmission] =
    tags2.foldLeft(if (tags1.isEmpty) tags1 else normalizedTags(Seq.empty, tags1)) {
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
