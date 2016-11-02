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

import scala.collection.immutable.VectorBuilder

import gnieh.diff._

/** The transformer is the core class of the lexicon generator. It takes a well-typed lexicon
 *  description and produces deterministic Fsts for lemmas, inflections and deflexions, depending on
 *  what need to be generated.
 *
 *  @author Lucas Satabin
 */
class Transformer(typer: Typer, diko: Diko) extends Phase[CompileOptions, Seq[GeneratedFile]](Some("transformer")) {

  private val lcs = new Patience[Char]

  // a regular expression that matches a non empty sequence of letters from the alphabet
  private val lettersRe =
    f"[${Regex.quote(diko.alphabet.mkString)}]+"

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

    // assume everything type-checks
    // first generate lemmas and inflections (no inversion of rules needed)
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
            (Word(str, gCat.orElse(eCat), gTags.normalizeWith(eTags))(w.offset) :: ws, rs)
          case ((ws, rs), r @ Rewrite(name, eTags, rules)) =>
            (ws, Rewrite(name, gTags.normalizeWith(eTags), rules)(r.offset) :: rs)
        }

      for (Word(input, cat, tags) <- words) {
        val builders = Seq(lemmasBuilder, inflectionsBuilder).flatten
        val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
        createStates(inChars, outChars, cat.get, tags, builders)
      }

      for (Rewrite(name, tags, rules) <- rewrites) {
        // a rewrite rule applies all its patterns in order to the words in this
        // lexicon (collected aboved). The first pattern that applies to a word
        // is the one taken, and the replacement is substituted to the word
        val rewrittenWords = rewriteWords(name, words, tags, rules)(reporter)
        for (Word(input, cat, tags) <- rewrittenWords) {
          val builders = inflectionsBuilder.toSeq
          val (inChars, outChars) = input.toVector.unzip { case WordChar(in, out) => (in, out) }
          createStates(inChars, outChars, cat.get, tags, builders)
        }

        // invert the rule and add it to the deflexions
        // only non recursive cases are handled, emit a warning for each recursive case
        for {
          (builder, i) <- deflexionsBuilder
          patterns <- rules
          (p @ Pattern(pattern, pCat, pTags), Replacement(repl, rTags)) <- patterns
        } {
          if (repl.exists(_ == RecursiveReplacement)) {
            reporter.warning("Cannot invert case with recusrive replacement, ignoring it", p.offset)
          } else {
            val tags1 = tags.normalizeWith(pTags).normalizeWith(rTags).collect { case (true, t) => TagOut(t) }
            val cat = gCat.orElse(pCat).map(CatOut)
            addDeflexion(pattern, cat, tags1, repl, builder, i, p.offset)(reporter)
          }
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

  private def createStates(inChars: Vector[Option[Char]], outChars: Vector[Option[Char]], cat: String, tags: Seq[TagEmission], builders: Seq[VectorBuilder[(Seq[Char], Seq[Out])]]): Unit = {
    val tags1 =
      normalizedTags(Seq.empty, tags).foldLeft(Seq.empty[TagOut]) {
        case (acc, (true, t)) if typer.isPublic(t) =>
          acc :+ TagOut(t)
        case (acc, _) =>
          acc
      }
    for (b <- builders) {
      b += (inChars.flatten -> ((outChars.flatten.map(CharOut(_)) :+ CatOut(cat)) ++ tags1))
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
      case (currentIdx, RecursiveReplacement) =>
        val newSeq = m.group(currentIdx).toSeq.map(c => WordChar(Some(c), Some(c)))
        val subword = Word(newSeq, category, Seq.empty)(offset)
        applyRule(rewriteName, subword, rule, Seq.empty, rule) match {
          case Some(w) => builder.append(w.word)
          case None    => builder.append(subword.word)
        }
        currentIdx + 1
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

  private def addDeflexion(pattern: Seq[CasePattern], cat: Option[CatOut], tags: Seq[TagOut], repl: Seq[CaseReplacement], builder: PNFst.Builder[Char, Out], init: PNFst.StateBuilder[Char, Out], offset: Int)(implicit reporter: Reporter): Unit = {
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
          reporter.error("Malformed rewrite rule", offset)
      }
    loop(init, pattern, repl, Seq())
  }

}
