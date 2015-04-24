package lingua
package grammar
package engine

object StdoutReporter extends Reporter {

  def abort(msg: String, position: Position) =
    sys.error(f"[error] $position $msg")

  def error(msg: String, position: Position): Unit =
    println(f"[error] $position $msg")

  def info(msg: String, position: Position): Unit =
    println(f"[info] $position $msg")

  def warning(msg: String, position: Position): Unit =
    println(f"[warning] $position $msg")

}
