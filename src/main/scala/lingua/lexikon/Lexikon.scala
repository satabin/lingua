package lingua
package lexikon

case class Lexikon(name: String, defaults: Option[Default], entries: Seq[Entry])

case class Default(category: Option[String], tags: Seq[(Boolean, String)])
