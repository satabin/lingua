package lingua
package lexikon
package fst

abstract class Fst[In, Out](val states: Set[State], val initials: Set[State], val finals: Set[State]) {

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    initials.contains(state)

  def toDot(transitions: Iterable[String]): String = {
    f"""digraph {
       |  ${
      states.map { s =>
        val shape = if (finals.contains(s)) "doublecircle" else "circle"
        f"q$s[shape=$shape]"
      }.mkString(";\n  ")
    }
       |  ${transitions.mkString(";\n  ")}
       |}""".stripMargin
  }

}
