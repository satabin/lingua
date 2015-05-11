package lingua
package lexikon

sealed trait Entry

final case class Transitions(transitions: Seq[Transition]) extends Entry

sealed trait Transition

final case class SimpleTransition(word: String) extends Transition

final case class InOutTransition(in: Option[Char], out: Option[Char], category: Option[String]) extends Transition

final case class TagTransition(present: Boolean, tag: String) extends Transition

final case class LexikonTransition(name: String) extends Transition

final case class RewriteRules(name: String, default: Default, cases: Seq[Seq[RewriteCase]]) extends Entry

final case class RewriteCase(inAffix: Option[Affix], trIn: Seq[Transition], outAffix: Option[Affix], trOut: Seq[Transition])

sealed trait Affix
case object Prefix extends Affix
case object Suffix extends Affix
