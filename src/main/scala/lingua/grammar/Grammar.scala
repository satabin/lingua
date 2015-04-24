package lingua
package grammar

case class Grammar(categories: Seq[Category], tags: Seq[Tag], start: (String, Position), productions: Seq[Production])
