package lingua

case class Position(file: String, line: Int, column: Int, start: Int, end: Int) {

  override def toString = f"$file:$line:$column:"

}
