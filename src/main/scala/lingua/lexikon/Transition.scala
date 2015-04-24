package lingua
package lexikon

sealed trait Entry

final case class Transitions(transitions: Seq[Transition]) extends Entry

sealed trait Transition

final case class SimpleTransition(word: String) extends Transition

final case class InOutTransition(in: Char, out: Char) extends Transition

final case class TagTransition(present: Boolean, tag: String) extends Transition

final case class RewriteRule(name: String, tags: Seq[(Boolean, String)], cases: Seq[RewriteCase]) extends Entry

final case class RewriteCase(tr1: Seq[Transition], tr2: Seq[Transition])
