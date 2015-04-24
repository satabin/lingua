package lingua
package grammar
package engine

sealed abstract class Sym[T <: Type] {
  val name: String
  val tpe: T
  val pos: Position

  override def toString =
    name
}

case object NoSym extends Sym[NoType.type] {
  val name = "??"
  val tpe = NoType
  val pos = NoPosition
}

case class CategorySym(name: String)(val explain: String, val pos: Position) extends Sym[CategoryType.type] {
  val tpe = CategoryType
}

sealed trait SomeTag extends Sym[TagType.type]

case class TagSym(name: String, group: Option[GroupSym])(val explain: String, val pos: Position) extends SomeTag {
  val tpe = TagType
}

case class ProductionSym(name: String, rhs: Vector[Rhs])(val pos: Position) extends Sym[ProductionType.type] {
  val tpe = ProductionType

  override def toString =
    f"$name"

  def printRule =
    f"$name = ${rhs.mkString(" ")}"

}

private case class UnresolvedProductionSym(name: String, rhs: Seq[grammar.Rhs])(val pos: Position) extends Sym[ProductionType.type] {
  val tpe = ProductionType
}

private case class ResolvingProductionSym(name: String)(val pos: Position) extends Sym[ProductionType.type] {
  val tpe = ProductionType

  private var _resolved: ProductionSym = null

  def resolved = _resolved

  private[engine] def resolved_=(sym: ProductionSym): Unit =
    _resolved = sym

}

case class GroupSym(name: String)(val pos: Position) extends SomeTag {
  val tpe = TagType
}
