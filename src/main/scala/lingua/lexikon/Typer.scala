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

import scala.collection.mutable.Map

class Typer(reporter: Reporter, diko: Diko) {

  private val categories = Map.empty[String, String]

  private val tags = Map.empty[String, (String, Boolean, Option[String])]

  private val charSet = diko.alphabet.toSet.union(diko.separators.toSet)

  private def isA(tag: String, name: String): Boolean =
    tag == name || tags.get(tag).flatMap(_._3.map(_ == name)).getOrElse(false)

  private def isAbstract(tag: String): Boolean =
    tags.get(tag).map(_._2).getOrElse(false)

  private def addCategory(cat: Category): Unit = categories.get(cat.alias) match {
    case Some(c) =>
      reporter.error(cat.offset, f"Category ${cat.alias} is already defined")
    case None =>
      categories(cat.alias) = cat.fullname
  }

  private def addTag(tag: Tag, parent: Option[String]): Unit = tags.get(tag.alias) match {
    case Some(t) =>
      reporter.error(tag.offset, f"Tag ${tag.alias} is already defined")
    case None =>
      tags(tag.alias) = (tag.fullname, tag.children.size > 0, parent)
      for (t <- tag.children)
        addTag(t, Some(tag.alias))
  }

  /** Resets the typing environment to the initial `diko` file. */
  def reset(): Unit = {
    for (cat <- diko.categories)
      addCategory(cat)
    for (tag <- diko.tags)
      addTag(tag, None)
  }

  reset()

  def typeCategory(category: Option[String], offset: Int): Unit =
    for {
      c <- category
      if !categories.contains(c)
    } {
      reporter.error(offset, f"Unknown category $c")
    }

  def typeEmissions(emissions: Seq[TagEmission], offset: Int): Unit =
    for {
      (_, t) <- emissions
      if !tags.contains(t)
    } {
      reporter.error(offset, f"Unknown tag $t")
    }

  def typeCheck(): Unit = {
    val Diko(_, _, _, _, lexika) = diko
    for (l <- lexika)
      typeLexikon(l)
  }

  def typeLexikon(lex: Lexikon): Unit = {
    val Lexikon(name, category, emissions, entries) = lex
    val offset = lex.offset

    typeCategory(category, offset)

    typeEmissions(emissions, offset)

    for (e <- entries)
      typeEntry(e)

  }

  def typeEntry(e: Entry): Unit = e match {
    case w @ Word(word, cat, emissions) =>
      // check that all characters in a word are either in the alphabet or a separator
      for {
        c <- word
        if !charSet.contains(c)
      } {
        reporter.error(w.offset, f"Unknown letter $c")
      }
      typeCategory(cat, w.offset)
      typeEmissions(emissions, w.offset)
    case r @ Rewrite(name, emissions, rules) =>
      typeEmissions(emissions, r.offset)
      for (r <- rules)
        typeRule(r)
  }

  def typeRule(r: Rule): Unit =
    for ((p @ Pattern(_, pattern, cat, pemissions), r @ Replacement(_, replacement, remissions)) <- r) {
      val poffset = p.offset
      val roffset = r.offset
      typeCategory(cat, poffset)
      typeEmissions(pemissions, poffset)
      typeEmissions(remissions, roffset)
      for (p <- pattern)
        typePattern(p, poffset)
      for (r <- replacement)
        typeReplacement(r, roffset)
    }

  def typePattern(p: CasePattern, offset: Int): Unit = p match {
    case CharPattern(c) if !charSet.contains(c) =>
      reporter.error(offset, f"Unknown letter $c")
    case _ =>
    // ok
  }

  def typeReplacement(r: CaseReplacement, offset: Int): Unit = r match {
    case CharReplacement(c) if !charSet.contains(c) =>
      reporter.error(offset, f"Unknown letter $c")
    case GroupReplacement(rs) =>
      for (r <- rs)
        typeReplacement(r, offset)
    case _ =>
    // ok
  }

}
