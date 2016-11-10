/* Copyright (c) 2015 Lucas Satabin
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
package untyped

/** A unit representing a file to compile.
 *
 *  @group Ast
 */
final case class DikoUnit(name: String, sections: Seq[Section])

case class Tag(public: Boolean, fullname: String, alias: String, children: Seq[Tag])(val offset: Int)

case class Category(fullname: String, alias: String)(val offset: Int)

/** A section in a linguistics file may be:
 *   - an alphabet section
 *   - a separator section
 *   - a tag section
 *   - a category section
 *   - a lexicon
 *   - a rewrite rule
 *
 *  @group Ast
 */
sealed trait Section

/** An alphabet section contains the alphabet letters.
 *  The order of letters in an alphabet section defines the lexicographical order in this alphabet.
 *
 *  @group Ast
 */
final case class Alphabet(letters: Seq[Char])(val offset: Int) extends Section

/** A separators section contains characters that may appear in words but are not letters.
 *  Separators do not participate to the lexicographical order.
 *
 *  @group Ast
 */
final case class Separators(separators: Seq[Char]) extends Section

/** A tags section contains [[Tag]] definitions.
 *  Tags may be grouped under the same name, so that they can be matched in rewrite rules by this name
 *  regarless of the concrete tag.
 *  Tags (and groups) may be private, meaning that they can be used in rewrite rules but will not appear
 *  in the output.
 *  Tags may be added and removed from any entry in a lexicon. An entry may have any number of tags (including none).
 *
 *  @group Ast
 */
final case class Tags(tags: Seq[Tag]) extends Section

/** A categories section contains [[Category]] definitions.
 *  Categories are mutually exclusive, and each entry must have exactly one category.
 *
 *  @group Ast
 */
final case class Categories(categories: Seq[Category]) extends Section

/** A lexicon groups [[Word]]s.
 *  A lexicon may define a [[Category]] and [[Tag]]s that are shared by all words defined in it.
 *
 *  @group Ast
 */
final case class Lexicon(name: String, category: Option[String], tags: Seq[TagEmission], words: Seq[Word])(val offset: Int) extends Section

/** A rewrite rule describes how words are rewritten to generate inflections.
 *  A rewrite rule matches on an optional [[Category]] and presence or absence of some [[Tag]]s in
 *  the input and potential tag emissions on all rewritten outputs.
 *  Each word that satisfies these criteria is eligible to be matched by lexical [[Case]]s of this rule.
 *  The list of [[Case]]s is ordered. The first matching [[Case]] is taken.
 *
 *  Typically it is a good practice to define a private [[Tag]], named e.g. `Exn`, to tag words that
 *  do not obey standard rules and to make each rewrite rule match on the absence of such a tag.
 *
 *  @group Ast
 */
final case class RewriteRule(name: String, category: Option[String], tagsin: Seq[TagEmission], tagsout: Seq[TagEmission], cases: Seq[Case])(val offset: Int) extends Section

/** A case defines a [[Pattern]] matching on the shape of a word and tags.
 *  If a word matches the patterns, then it is rewritten according to the [[Replacement]] part.
 *
 *  @group Ast
 */
final case class Case(pattern: Pattern, replacement: Replacement)

/** A word is a raw entry in the lexicon.
 *  It may be a lemma to be rewritten later by rewrite rules or an inflected form
 *  of a lemma that cannot be handled by rewrite rules due to some exception.
 *
 *  @group Ast
 */
final case class Word(input: Seq[WordChar], category: Option[String], tags: Seq[TagEmission])(val offset: Int) {

  val word = input.collect { case WordChar(Some(c), _) => c }.mkString

}

/** A pattern is two-fold:
 *   - a lexical pattern matching on the shape of a word
 *   - a tag pattern matching on present or absent tags of a word
 *  It matches if both patterns match.
 *
 *  @group Ast
 */
final case class Pattern(seq: Seq[CasePattern], category: Option[String], tags: Seq[TagEmission])(val offset: Int)

/** A replacement text, may use reference to some captured pattern or be a recursive
 *  call to the current rule.
 *
 *  @group Ast
 */
final case class Replacement(seq: Seq[CaseReplacement], tags: Seq[TagEmission])(val offset: Int)
