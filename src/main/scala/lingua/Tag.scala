package lingua

case class Tag(fullname: String, alias: String, group: Option[String])(val pos: Position)
