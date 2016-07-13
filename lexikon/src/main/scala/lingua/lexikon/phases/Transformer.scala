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
class Transformer(typer: Typer, diko: Diko) extends Phase[CompileOptions, NFst[Char, Out]](Some("transformer")) {

  private val lcs = new Patience[Char]

  private val fstBuilder = NFst.Builder.create[Char, Out]

  private var fst: NFst[Char, Out] = null

  // a regular expression that matches a non empty sequence of letters from the alphabet
  private val lettersRe =
    f"[${Regex.quote(diko.alphabet.mkString)}]+"

  def process(options: CompileOptions, reporter: Reporter): NFst[Char, Out] = {
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
          val rewrittenWords = rewriteWords(name, words, tags, rules)(reporter)
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
  private def createStates(inChars: Vector[Option[Char]], outChars: Vector[Option[Char]], cat: String, tags: Seq[TagEmission], idx: Int, previous: NFst.StateBuilder[Char, Out]): Unit = {
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
      previous.makeFinal.addFinalOutput(tags1 :+ CatOut(cat))
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

  private def rewriteWords(rewriteName: String, words: List[Word], rTags: Seq[TagEmission], rules: Seq[Rule])(implicit reporter: Reporter): List[Word] =
    for {
      word <- words
      rule <- rules
      rewritten <- applyRule(rewriteName, word, rule, rTags, rule)
    } yield rewritten

  private def applyPattern(rewriteName: String, word: Word, rule: Rule, rTags: Seq[TagEmission], pattern: Pattern, replacement: Replacement)(implicit reporter: Reporter): Option[Word] = {
    val Pattern(seq, category, tags) = pattern
    val (mustTags, mustntTags) =
      tags.foldLeft((Set.empty[String], Set.empty[String])) {
        case ((mustTags, mustntTags), (true, tag))  => (mustTags + tag, mustntTags)
        case ((mustTags, mustntTags), (false, tag)) => (mustTags, mustntTags + tag)
      }

    val compiledPattern = compilePattern(seq)
    rewriteWord(rewriteName, word, rule, compiledPattern, category, mustTags, mustntTags, rTags, replacement)
  }

  private def applyRule(rewriteName: String, word: Word, origin: Rule, rTags: Seq[TagEmission], rule: Rule)(implicit reporter: Reporter): Option[Word] =
    if (rule.isEmpty) {
      None
    } else {
      val (pat, repl) = rule.head
      applyPattern(rewriteName, word, origin, rTags, pat, repl).orElse(applyRule(rewriteName, word, origin, rTags, rule.tail))
    }

  private def buildString(rewriteName: String, offset: Int, category: Option[String], rule: Rule, m: Match, currentIdx: Int, seq: Seq[CaseReplacement], builder: StringBuilder)(implicit reporter: Reporter): Int =
    seq.foldLeft(currentIdx) {
      case (currentIdx, StringReplacement(s)) =>
        builder.append(s)
        currentIdx
      case (currentIdx, CaptureReplacement) =>
        builder.append(m.group(currentIdx))
        currentIdx + 1
      case (currentIdx, RecursiveReplacement(seq)) =>
        val substring = new StringBuilder
        val currentIdx1 = buildString(rewriteName, offset, category, rule, m, currentIdx, seq, substring)
        val newSeq = substring.toSeq.map(c => WordChar(Some(c), Some(c)))
        val subword = Word(newSeq, category, Seq.empty)(offset)
        applyRule(rewriteName, subword, rule, Seq.empty, rule) match {
          case Some(w) => builder.append(w.word)
          case None    => builder.append(subword.word)
        }
        currentIdx1
    }

  private def rewriteWord(rewriteName: String, original: Word, rule: Rule, pattern: Regex, mustCategory: Option[String], mustTags: Set[String], mustntTags: Set[String], rTags: Seq[TagEmission], replacement: Replacement)(implicit reporter: Reporter): Option[Word] = {
    val normalized = normalizedTags(Seq.empty, original.tags)
    if ((mustCategory.isEmpty || mustCategory == original.category) && mustTags.forall(t => normalized.exists(tag => typer.isA(tag, t))) && mustntTags.forall(t => !normalized.exists(tag => typer.isA(tag, t)))) {
      val normalized1 = normalizedTags(normalized, replacement.tags)
      for (m <- pattern.findFirstMatchIn(original.word)) yield {
        // build the new word based on original and replacement text
        val builder = new StringBuilder
        buildString(rewriteName, original.offset, original.category, rule, m, 1, replacement.seq, builder)
        if (m.end < original.word.size)
          builder.append(original.word.substring(m.end, original.word.size))
        val chars = buildWordChars(original.word, builder.toString)
        val res = Word(chars, original.category, normalizedTags(normalized1, rTags))(original.offset)
        reporter.verbose(f"Word ${original.word} rewritten into ${res.word} by rule $rewriteName")
        res
      }
    } else {
      None
    }
  }

  private def compilePattern(pattern: Seq[CasePattern]): Regex = {
    val builder = new StringBuilder
    builder.append("^")
    val compiledPattern =
      pattern.foldLeft(builder) {
        case (acc, StringPattern(s)) =>
          acc.append(Regex.quote(s))
        case (acc, CapturePattern) =>
          acc.append(f"($lettersRe)")
      }
    compiledPattern.append("$")
    compiledPattern.toString.r
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
