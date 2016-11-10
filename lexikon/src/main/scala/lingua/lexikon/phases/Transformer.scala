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
import typed._

import scala.annotation.tailrec

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

import scala.collection.immutable.VectorBuilder

import gnieh.diff._

/** The transformer is the core class of the lexicon generator. It takes a well-typed lexicon
 *  description and produces deterministic Fsts for lemmas, inflections and deflexions, depending on
 *  what need to be generated.
 *
 *  @author Lucas Satabin
 */
class Transformer(typed: Diko) extends Phase[CompileOptions, Seq[GeneratedFile]](Some("transformer")) {

  private val lcs = new Patience[Char]

  // a regular expression that matches a non empty sequence of letters from the alphabet
  private val lettersRe =
    f"[${Regex.quote(typed.alphabet.mkString)}]+"

  def process(options: CompileOptions, reporter: Reporter): Seq[GeneratedFile] = {

    val lemmasBuilder = if (options.generateLemmas) Some(new VectorBuilder[(Seq[Char], Seq[Out])]) else None
    val inflectionsBuilder = if (options.generateInflections) Some(new VectorBuilder[(Seq[Char], Seq[Out])]) else None
    val deflexionsBuilder =
      if (options.generateDeflexions) {
        val b = PNFst.Builder.create[Char, Out]
        val i = b.newState.makeInitial
        Some(b -> i)
      } else {
        None
      }

    for (Word(input, cat, tags, lname) <- typed.words) {
      val builders = Seq(lemmasBuilder, inflectionsBuilder).flatten
      val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
      addInflection(inChars, outChars, cat, tags, builders)
    }

    for (rewrite @ RewriteRule(_, cases) <- typed.rewrites) {

      // a rewrite rule applies all its patterns in order to the words in this
      // lexicon (collected above). The first pattern that applies to a word
      // is the one taken, and the replacement is substituted to the word
      for (builder <- inflectionsBuilder) {
        val rewrittenWords = rewriteWords(rewrite, typed.words, reporter)
        for (Word(input, cat, tags, lname) <- rewrittenWords) {
          val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
          addInflection(inChars, outChars, cat, tags, Seq(builder))
        }
      }

      // invert the rule and add it to the deflexions
      // only non recursive cases are handled, emit a warning for each recursive case
      for {
        (builder, i) <- deflexionsBuilder
        Case(p @ Pattern(pattern, cat, tags), Replacement(repl, rTags)) <- cases
      } {
        if (repl.exists(_ == RecursiveReplacement)) {
          reporter.warning("Cannot invert case with recusrive replacement, ignoring it", p.uname, p.offset)
        } else {
          val tags1 = tags.collect { case (true, t @ ConcreteTag(name, _, _, _)) => t }
          val tags2 = normalizeWith(tags1, rTags).map(t => TagOut(t.alias))
          addDeflexion(pattern, cat.map(c => CatOut(c.alias)), tags2, repl, builder, i, p.uname, p.offset, reporter)
        }
      }

    }

    // build the NFst
    val deflexionsNFst =
      for ((b, _) <- deflexionsBuilder) yield b.build()

    // generate dot files for NFsts if asked to
    val deflexionsNDot =
      for (nfst <- deflexionsNFst if options.saveNFst) yield DotFile(options.outputDir / f"${options.deflexionsFile}-nfst.dot", nfst.toDot)

    // determinize NFsts to get the Fsts and generate Fst files
    val lemmasFst =
      for (b <- lemmasBuilder)
        yield PSubFstFile(options.outputDir / f"${options.lemmasFile}.diko", PSubFst.Builder.fromEntries(b.result))
    val inflectionsFst =
      for (b <- inflectionsBuilder)
        yield PSubFstFile(options.outputDir / f"${options.inflectionsFile}.diko", PSubFst.Builder.fromEntries(b.result))
    val deflexionsFst =
      for (nfst <- deflexionsNFst)
        yield QPFstFile(options.outputDir / f"${options.deflexionsFile}.diko", nfst.determinize)

    // generated dot files for Fsts if asked to
    val lemmasDot =
      for (PSubFstFile(_, fst) <- lemmasFst if options.saveFst) yield DotFile(options.outputDir / f"${options.lemmasFile}-fst.dot", fst.toDot)
    val inflectionsDot =
      for (PSubFstFile(_, fst) <- inflectionsFst if options.saveFst) yield DotFile(options.outputDir / f"${options.inflectionsFile}-fst.dot", fst.toDot)
    val deflexionsDot =
      for (QPFstFile(_, fst) <- deflexionsFst if options.saveFst) yield DotFile(options.outputDir / f"${options.deflexionsFile}-fst.dot", fst.toDot)

    Seq(lemmasFst, inflectionsFst, deflexionsFst, deflexionsNDot, lemmasDot, inflectionsDot, deflexionsDot).flatten
  }

  private def addInflection(inChars: Vector[Option[Char]], outChars: Vector[Option[Char]], cat: Category, tags: Seq[Tag], builders: Seq[VectorBuilder[(Seq[Char], Seq[Out])]]): Unit = {
    for (b <- builders) {
      b += (inChars.flatten -> ((outChars.flatten.map(CharOut(_)) :+ CatOut(cat.alias)) ++ tags.map(t => TagOut(t.alias))))
    }
  }

  private def rewriteWords(rewrite: RewriteRule, words: Set[Word], reporter: Reporter): Set[Word] =
    for {
      word <- words
      rewritten <- applyCases(rewrite, word, rewrite.cases, reporter)
    } yield rewritten

  private def applyPattern(rewrite: RewriteRule, word: Word, cases: Seq[Case], pattern: Pattern, replacement: Replacement, reporter: Reporter): Option[Word] = {
    val Pattern(seq, cat, tags) = pattern
    val compiledPattern = compilePattern(seq)
    rewriteWord(rewrite, word, cases, compiledPattern, cat, tags, replacement, reporter)
  }

  private def applyCases(rewrite: RewriteRule, word: Word, cases: Seq[Case], reporter: Reporter): Option[Word] =
    cases match {
      case Seq() =>
        None
      case Seq(Case(pat, repl), rest @ _*) =>
        applyPattern(rewrite, word, cases, pat, repl, reporter).orElse(applyCases(rewrite, word, rest, reporter))
    }

  private def buildString(rewrite: RewriteRule, offset: Int, category: Category, cases: Seq[Case], m: Match, currentIdx: Int, seq: Seq[CaseReplacement], builder: StringBuilder, uname: String, reporter: Reporter): Int =
    seq.foldLeft(currentIdx) {
      case (currentIdx, StringReplacement(s)) =>
        builder.append(s)
        currentIdx
      case (currentIdx, CaptureReplacement) =>
        builder.append(m.group(currentIdx))
        currentIdx + 1
      case (currentIdx, RecursiveReplacement) =>
        val newSeq = m.group(currentIdx).toSeq.map(c => WordChar(Some(c), Some(c)))
        val subword = Word(newSeq, category, Seq.empty, "")(uname, offset)
        applyCases(rewrite, subword, rewrite.cases, reporter) match {
          case Some(w) => builder.append(w.word)
          case None    => builder.append(subword.word)
        }
        currentIdx + 1
    }

  private def rewriteWord(rewrite: RewriteRule, original: Word, cases: Seq[Case], pattern: Regex, mustCategory: Option[Category], mustEmissions: Seq[TagEmission], replacement: Replacement, reporter: Reporter): Option[Word] = {
    val Word(_, cat, tags, lname) = original
    if ((mustCategory.isEmpty || mustCategory == Some(cat)) && mustEmissions.forall { case (present, tgt) => tags.exists(t => typed.isA(tgt, t)) == present }) {
      for (m <- pattern.findFirstMatchIn(original.word)) yield {
        // build the new word based on original and replacement text
        val builder = new StringBuilder
        buildString(rewrite, original.offset, cat, cases, m, 1, replacement.seq, builder, original.uname, reporter)
        if (m.end < original.word.size)
          builder.append(original.word.substring(m.end, original.word.size))
        val chars = buildWordChars(original.word, builder.toString)
        val rewrittentags = normalizeWith(tags, replacement.tags)
        val res = Word(chars, original.category, rewrittentags, f"$lname (rewritten)")(original.uname, original.offset)
        reporter.verbose(f"Word ${original.word} rewritten into ${res.word} by rule ${rewrite.name}")
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

  private def addDeflexion(pattern: Seq[CasePattern], cat: Option[CatOut], tags: Seq[TagOut], repl: Seq[CaseReplacement], builder: PNFst.Builder[Char, Out], init: PNFst.StateBuilder[Char, Out], uname: String, offset: Int, reporter: Reporter): Unit = {
    @tailrec
    def loop(last: PNFst.StateBuilder[Char, Out], pattern: Seq[CasePattern], repl: Seq[CaseReplacement], accOut: Seq[Out]): Unit =
      (pattern, repl) match {
        case (Seq(StringPattern(pat), pattern1 @ _*), Seq(StringReplacement(rep), repl1 @ _*)) =>
          val word = buildWordChars(pat, rep)
          val (outState, accOut1) = word.foldLeft(last -> accOut) {
            case ((st, accOut), WordChar(Some(in), Some(out))) =>
              val st1 = builder.newState
              st.addTransition(Predicate(in), (accOut :+ CharOut(out)).map(Predicate(_) -> false), st1)
              st1 -> Seq.empty[Out]
            case ((st, accOut), WordChar(Some(in), None)) =>
              val st1 = builder.newState
              st.addTransition(Predicate(in), accOut.map(Predicate(_) -> false), st1)
              st1 -> Seq.empty[Out]
            case ((st, accOut), WordChar(None, Some(out))) =>
              st -> (accOut :+ CharOut(out))
            case (acc, WordChar(None, None)) =>
              acc
          }
          loop(outState, pattern1, repl1, accOut1)
        case (Seq(StringPattern(pat), pattern1 @ _*), _) =>
          loop(last, pattern1, repl, accOut ++ pat.map(CharOut(_)))
        case (_, Seq(StringReplacement(rep), repl1 @ _*)) =>
          val outState = rep.foldLeft(last) { (st, c) =>
            val st1 = builder.newState
            st.addTransition(Predicate(c), Seq(), st1)
            st1
          }
          loop(outState, pattern, repl1, accOut)
        case (Seq(CapturePattern, pattern1 @ _*), Seq(CaptureReplacement, repl1 @ _*)) =>
          last.addTransition(AnyPredicate, accOut.map(Predicate(_) -> false) :+ (AnyPredicate -> true), last)
          loop(last, pattern1, repl1, Seq())
        case (Seq(), Seq()) =>
          last.makeFinal.addFinalOutput(accOut ++ cat.toSeq ++ tags)
        case (_, _) =>
          reporter.error("Malformed rewrite rule", uname, offset)
      }
    loop(init, pattern, repl, Seq())
  }

  def normalizeWith(tags: Seq[ConcreteTag], tags1: Seq[TagEmission]): Seq[ConcreteTag] =
    tags1.foldLeft(tags) {
      case (acc, (true, tag @ ConcreteTag(_, _, _, _))) if !acc.exists(_ == tag) =>
        acc :+ tag
      case (acc, (false, tag)) if acc.exists(typed.isA(tag, _)) =>
        acc.filterNot(typed.isA(tag, _))
      case (acc, tage) =>
        acc
    }

}
