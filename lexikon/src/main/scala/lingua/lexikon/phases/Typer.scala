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

import scala.collection.mutable.Map

/** The typer builds a typing environment for an input lexicon description.
 *  This environment can then be used to retrieve various information.
 *
 *  @author Lucas Satabin
 */
class Typer(diko: Diko) extends Phase[CompileOptions, Typer](Some("typer")) {

  private val categories = Map.empty[String, String]

  private val tags = Map.empty[String, (String, Boolean, Option[String], Boolean)]

  private val charSet = diko.alphabet.toSet.union(diko.separators.toSet)

  /** Indicates whether the tag emission emits a tag of the given type. */
  def isA(tagEmission: (Boolean, String), name: String): Boolean = {
    val (present, tag) = tagEmission
    present && (tag == name || tags.get(tag).flatMap(_._3.map(_ == name)).getOrElse(false))
  }

  /** Indicates whether the tag is abstract. */
  def isAbstract(tag: String): Boolean =
    tags.get(tag).map(_._2).getOrElse(false)

  /** Indicates whether the tag is public. */
  def isPublic(tag: String): Boolean =
    tags.get(tag).map(_._4).getOrElse(false)

  def addCategory(cat: Category)(implicit reporter: Reporter): Unit = categories.get(cat.alias) match {
    case Some(c) =>
      reporter.error(f"Category ${cat.alias} is already defined", cat.offset)
    case None =>
      categories(cat.alias) = cat.fullname
  }

  def addTag(tag: Tag, parent: Option[String])(implicit reporter: Reporter): Unit = tags.get(tag.alias) match {
    case Some(t) =>
      reporter.error(f"Tag ${tag.alias} is already defined", tag.offset)
    case None =>
      val public = parent.fold(tag.public)(p => tag.public && tags(p)._4)
      tags(tag.alias) = (tag.fullname, tag.children.size > 0, parent, public)
      for (t <- tag.children)
        addTag(t, Some(tag.alias))
  }

  /** Resets the typing environment to the initial `diko` file. */
  def reset(implicit reporter: Reporter): Unit = {
    for (cat <- diko.categories)
      addCategory(cat)
    for (tag <- diko.tags)
      addTag(tag, None)
  }

  def typeCategory(category: Option[String], offset: Int)(implicit reporter: Reporter): Unit =
    for {
      c <- category
      if !categories.contains(c)
    } {
      reporter.error(f"Unknown category $c", offset)
    }

  def typeEmissions(emissions: Seq[TagEmission], offset: Int)(implicit reporter: Reporter): Unit =
    for {
      (_, t) <- emissions
      if !tags.contains(t)
    } {
      reporter.error(f"Unknown tag $t", offset)
    }

  /** Type checks the entire lexicon description file. */
  def process(options: CompileOptions, reporter: Reporter): Typer = {
    reset(reporter)
    val Diko(_, _, _, _, lexika) = diko
    for (l <- lexika)
      typeLexikon(l)(reporter)
    this
  }

  def typeLexikon(lex: Lexikon)(implicit reporter: Reporter): Unit = {
    val Lexikon(name, category, emissions, entries) = lex
    val offset = lex.offset

    typeCategory(category, offset)

    typeEmissions(emissions, offset)

    for (e <- entries)
      typeEntry(e)

  }

  def typeEntry(e: Entry)(implicit reporter: Reporter): Unit = e match {
    case w @ Word(word, cat, emissions) =>
      // check that all characters in a word are either in the alphabet or a separator
      for (WordChar(c1, c2) <- word) {
        for {
          c <- c1
          if !charSet.contains(c)
        } reporter.error(f"Unknown letter $c", w.offset)
        for {
          c <- c2
          if !charSet.contains(c)
        } reporter.error(f"Unknown letter $c", w.offset)
      }
      typeCategory(cat, w.offset)
      typeEmissions(emissions, w.offset)
    case r @ Rewrite(name, emissions, rules) =>
      typeEmissions(emissions, r.offset)
      for (r <- rules)
        typeRule(name, r)
  }

  def typeRule(rewriteName: String, r: Rule)(implicit reporter: Reporter): Unit =
    for ((p @ Pattern(pattern, cat, pemissions), r @ Replacement(replacement, remissions)) <- r) {
      val poffset = p.offset
      val roffset = r.offset
      typeCategory(cat, poffset)
      typeEmissions(pemissions, poffset)
      typeEmissions(remissions, roffset)
      for (p <- pattern)
        typePattern(p, poffset)
      for (r <- replacement)
        typeReplacement(rewriteName, r, roffset)
    }

  def typePattern(p: CasePattern, offset: Int)(implicit reporter: Reporter): Unit = p match {
    case StringPattern(s) =>
      for {
        c <- s
        if !charSet.contains(c)
      } reporter.error(f"Unknown letter $c", offset)
    case _ =>
    // ok
  }

  def typeReplacement(rewriteName: String, r: CaseReplacement, offset: Int)(implicit reporter: Reporter): Unit = r match {
    case StringReplacement(s) =>
      for {
        c <- s
        if !charSet.contains(c)
      } reporter.error(f"Unknown letter $c", offset)
    case RecursiveReplacement(rs) =>
      for (r <- rs)
        typeReplacement(rewriteName, r, offset)
    case _ =>
    // ok
  }

}
