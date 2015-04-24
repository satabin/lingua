package lingua
package grammar

case class Production(name: String, rhs: Seq[Rhs])(val pos: Position)
