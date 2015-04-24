package lingua
package grammar
package engine

sealed trait Rhs

case class Alternative(alts: Vector[Vector[Rhs]]) extends Rhs {
  override def toString =
    f"[ ${alts.map(_.mkString(" ")).mkString(" | ")} ]"
}

case class Optional(rhs: Vector[Rhs]) extends Rhs {
  override def toString =
    f"(${rhs.mkString(" ")})"
}

sealed trait Ref extends Rhs

case class Literal(value: String) extends Ref {
  override def toString =
    f""""$value""""
}

case class CategoryRef(name: CategorySym, tags: Vector[(Boolean, TagSym)]) extends Ref {
  override def toString = {
    val tagss =
      if (tags.isEmpty)
        ""
      else
        tags.map { case (present, TagSym(name, _)) => f"${if (present) "+" else "-"}$name" }.mkString("{", ", ", "}")
    f"${name}${tagss}"
  }
}

class ProdRef(_name: => ProductionSym, val tags: Vector[(Boolean, TagSym)]) extends Ref {

  lazy val name = _name

  override def toString = {
    val tagss =
      if (tags.isEmpty)
        ""
      else
        tags.map { case (present, TagSym(name, _)) => f"${if (present) "+" else "-"}$name" }.mkString("{", ", ", "}")
    f"${name}${tagss}"
  }
}
object ProdRef {

  def apply(name: => ProductionSym, tags: Vector[(Boolean, TagSym)]): ProdRef =
    new ProdRef(name, tags)

  def unapply(rhs: Rhs): Option[(ProductionSym, Vector[(Boolean, TagSym)])] = rhs match {
    case prod: ProdRef => Some(prod.name -> prod.tags)
    case _             => None
  }

}

case class Abstract(name: CategorySym, inherited: Vector[Sym[TagType.type]]) extends Ref {

  override def toString =
    f"$name <-{${inherited.mkString(", ")}}-"

}

private case object ErrorRef extends Ref {
  override def toString =
    "??"
}
