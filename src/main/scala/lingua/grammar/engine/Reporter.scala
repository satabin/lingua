package lingua
package grammar
package engine

trait Reporter {

  def info(msg: String, position: Position): Unit

  def warning(msg: String, position: Position): Unit

  def error(msg: String, position: Position): Unit

  def abort(msg: String, position: Position): Nothing

}
