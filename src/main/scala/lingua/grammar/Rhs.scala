package lingua
package grammar

sealed trait Rhs

case class Alternative(alts: Seq[Seq[Rhs]]) extends Rhs

case class Optional(rhs: Seq[Rhs]) extends Rhs

case class Literal(value: String) extends Rhs

case class Symbol(name: String, tags: Set[(Boolean, String)]) extends Rhs

case class Abstract(name: String, inherited: Seq[String]) extends Rhs
