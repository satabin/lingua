package lingua
package grammar
package engine

sealed trait Type

case object NoType extends Type {
  override def toString = "??"
}
case object CategoryType extends Type {
  override def toString = "category"
}
case object TagType extends Type {
  override def toString = "tag"
}
case object ProductionType extends Type {
  override def toString = "production"
}
