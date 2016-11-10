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

import untyped._

import typed.{
  AbstractTag,
  Case => TCase,
  Category => TCategory,
  ConcreteTag,
  Diko => TDiko,
  Pattern => TPattern,
  Replacement => TReplacement,
  RewriteRule => TRewriteRule,
  Tag => TTag,
  TagEmission => TTagEmission,
  Word => TWord
}

import scala.annotation.tailrec

/** The typer builds a typing environment for an input lexicon description.
 *  This environment can then be used to retrieve various information.
 *
 *  @author Lucas Satabin
 */
class Typer(units: Seq[DikoUnit]) extends Phase[CompileOptions, TDiko](Some("typer")) {

  private case class Diko(alphabet: Seq[Char], separators: Seq[Char], tags: Set[(Tag, String)], categories: Set[(Category, String)], lexica: Set[(Lexicon, String)], rewrites: Set[(RewriteRule, String)])

  /** Type checks the entire lexicon description file. */
  def process(options: CompileOptions, reporter: Reporter): TDiko = {
    // first collect the different sections with their containing units
    val diko: Diko = units.foldLeft(Diko(Seq(), Seq(), Set(), Set(), Set(), Set())) {
      case (acc, DikoUnit(uname, sections)) =>
        sections.foldLeft(acc) {
          case (d @ Diko(Seq(), _, _, _, _, _), Alphabet(chars)) =>
            d.copy(alphabet = chars)
          case (d, a @ Alphabet(_)) =>
            reporter.error(f"Duplicate alphabet section", uname, a.offset)
            d
          case (d @ Diko(_, separators, _, _, _, _), Separators(seps)) =>
            d.copy(separators = separators ++ seps)
          case (d @ Diko(_, _, tags, _, _, _), Tags(ts)) =>
            d.copy(tags = tags ++ ts.map(_ -> uname))
          case (d @ Diko(_, _, _, categories, _, _), Categories(cs)) =>
            d.copy(categories = categories ++ cs.map(_ -> uname))
          case (d @ Diko(_, _, _, _, lexica, _), l @ Lexicon(_, _, _, _)) =>
            d.copy(lexica = lexica + (l -> uname))
          case (d @ Diko(_, _, _, _, _, rewrites), r @ RewriteRule(_, _, _, _, _)) =>
            d.copy(rewrites = rewrites + (r -> uname))
        }
    }
    val Diko(alphabet, separators, tags, categories, lexica, rewrites) = diko
    val charSet = alphabet.toSet ++ separators.toSet

    val ttags =
      tags.foldLeft(Map.empty[String, TTag]) {
        case (ttags, (tag @ Tag(public, fullname, alias, children), uname)) =>
          ttags.get(alias) match {
            case Some(_) =>
              reporter.error(f"Tag $alias is already defined", uname, tag.offset)
              ttags
            case None =>
              if (tag.children.isEmpty) {
                ttags.updated(alias, ConcreteTag(alias, fullname, public, None)(uname, tag.offset))
              } else {
                val parent = AbstractTag(alias, fullname, public)(uname, tag.offset)
                children.foldLeft(ttags.updated(alias, parent)) {
                  case (ttags, tag @ Tag(public, fullname, alias, children)) =>
                    ttags.updated(alias, ConcreteTag(alias, fullname, public, Some(parent))(uname, tag.offset))
                }
              }
          }
      }

    lazy val tcategories =
      categories.foldLeft(Map.empty[String, TCategory]) {
        case (tcategories, (cat @ Category(fullname, alias), uname)) =>
          tcategories.get(alias) match {
            case Some(_) =>
              reporter.error(f"Category $alias is already defined", uname, cat.offset)
              tcategories
            case None =>
              tcategories.updated(alias, TCategory(alias, fullname)(uname, cat.offset))
          }
      }

    lazy val twords =
      lexica.foldLeft(Set.empty[TWord]) {
        case (twords, (lex, uname)) => typeLexicon(lex, uname, twords)
      }

    lazy val trewrites =
      rewrites.foldLeft(Set.empty[TRewriteRule]) {
        case (trewrites, (rewrite, uname)) => trewrites + typeRewriteRule(rewrite, uname)
      }

    def typeCategory(category: Option[String], uname: String, offset: Int): Option[TCategory] =
      category.flatMap(c => tcategories.get(c) match {
        case tc @ Some(_) => tc
        case None =>
          reporter.error(f"Uknown category $c", uname, offset)
          None
      })

    def typeEmissions(emissions: Seq[TagEmission], uname: String, offset: Int): Seq[TTagEmission] =
      emissions.flatMap {
        case (present, t) =>
          ttags.get(t) match {
            case Some(tt) => Some((present, tt))
            case None =>
              reporter.error(f"Unknown tag $t", uname, offset)
              None
          }
      }

    def typeCategoryOverload(cat: Option[String], parent: Option[TCategory], catRequired: Boolean, uname: String, offset: Int): Option[TCategory] = (typeCategory(cat, uname, offset), parent) match {
      case (Some(c1), Some(c2)) if c1 == c2 =>
        parent
      case (Some(TCategory(c1, _)), Some(TCategory(c2, _))) =>
        reporter.error(f"Duplicate category: $c1 is not equal to $c2", uname, offset)
        None
      case (c @ Some(_), None) =>
        c
      case (None, c @ Some(_)) =>
        c
      case (None, None) =>
        if (catRequired)
          reporter.error(f"No category defined", uname, offset)
        None
    }

    def typeLexicon(lex: Lexicon, uname: String, twords: Set[TWord]): Set[TWord] = {
      val Lexicon(lname, category, emissions, words) = lex
      val offset = lex.offset

      val tcat = typeCategory(category, uname, offset)

      val temissions = typeEmissions(emissions, uname, offset)
      words.foldLeft(twords) {
        case (twords, w) => typeWord(w, tcat, temissions, lname, uname).fold(twords)(twords + _)
      }
    }

    def typeWord(w: Word, lcat: Option[TCategory], ltags: Seq[TTagEmission], lname: String, uname: String): Option[TWord] = {
      val Word(word, cat, emissions) = w
      // check that all characters in a word are either in the alphabet or a separator
      val charsOk = word.forall {
        case WordChar(c1, c2) =>
          val c1Ok = c1.fold(true) { c =>
            if (!charSet.contains(c))
              reporter.error(f"Unknown letter $c", uname, w.offset)
            charSet.contains(c)
          }
          val c2Ok = c2.fold(true) { c =>
            if (!charSet.contains(c))
              reporter.error(f"Unknown letter $c", uname, w.offset)
            charSet.contains(c)
          }
          c1Ok && c2Ok
      }

      if (charsOk) {
        // ensure that there is exactly one category for the word
        val tcat = typeCategoryOverload(cat, lcat, true, uname, w.offset)
        val temissions = typeEmissions(emissions, uname, w.offset).normalizeWith(ltags)
        tcat.map(c => TWord(word, c, temissions.collect { case (_, t @ ConcreteTag(_, _, _, _)) => t }, lname)(uname, w.offset))
      } else {
        None
      }
    }

    def typeRewriteRule(r: RewriteRule, uname: String): TRewriteRule = {
      val RewriteRule(rname, cat, tagsin, tagsout, cases) = r
      val tcategory = typeCategory(cat, uname, r.offset)
      val ttagsin = typeEmissions(tagsin, uname, r.offset)
      val ttagsout = typeEmissions(tagsout, uname, r.offset)
      val tcases = cases.map(typeCase(tcategory, ttagsin, ttagsout, _, uname))
      TRewriteRule(rname, tcases)(uname, r.offset)
    }

    def typeCase(rcat: Option[TCategory], rtagsin: Seq[TTagEmission], rtagsout: Seq[TTagEmission], c: Case, uname: String): TCase = {
      val Case(p, r) = c
      val (nbCaptures, tpattern) = typePattern(p, rcat, rtagsin, uname)
      val treplacement = typeReplacement(r, nbCaptures, rtagsout, uname)
      TCase(tpattern, treplacement)
    }

    def typePattern(p: Pattern, rcat: Option[TCategory], rtags: Seq[TTagEmission], uname: String): (Int, TPattern) = {
      val Pattern(ps, cat, tags) = p
      val nbCaptures = ps.foldLeft(0) {
        case (acc, CapturePattern) => acc + 1
        case (acc, _)              => acc
      }
      for {
        StringPattern(s) <- ps
        c <- s
        if !charSet.contains(c)
      } reporter.error(f"Unknown letter $c", uname, p.offset)
      val tcategory = typeCategoryOverload(cat, rcat, false, uname, p.offset)
      val temissions = typeEmissions(tags, uname, p.offset).normalizeWith(rtags)
      (nbCaptures, TPattern(ps, tcategory, temissions)(uname, p.offset))
    }

    def typeReplacement(r: Replacement, nbCaptures: Int, rtags: Seq[TTagEmission], uname: String): TReplacement = {
      val Replacement(rs, tags) = r
      val replCaptures = rs.foldLeft(0) {
        case (acc, CaptureReplacement)   => acc + 1
        case (acc, RecursiveReplacement) => acc + 1
        case (acc, _)                    => acc
      }
      for {
        StringReplacement(s) <- rs
        c <- s
        if !charSet.contains(c)
      } reporter.error(f"Unknown letter $c", uname, r.offset)
      if (options.generateInflections && replCaptures > nbCaptures) {
        reporter.error("Replacement has more captures than pattern and cannot be used to generate inflections", uname, r.offset)
      }
      if (options.generateDeflexions && nbCaptures > replCaptures) {
        reporter.error("Pattern has more captures than replacement and cannot be used to generate deflexions", uname, r.offset)
      }
      val temissions = typeEmissions(tags, uname, r.offset).normalizeWith(rtags)
      TReplacement(rs, temissions)(uname, r.offset)
    }

    TDiko(alphabet, separators, tcategories, ttags, twords, trewrites)

  }

}
