package lingua
package lexikon
package fst

/** A p-subsequential finite-state transducer. */
class PSubFst[In, Out] private (states: Set[State],
    initial: State,
    finals: Set[State],
    maps: (Map[(State, In), State], Map[(State, In), Seq[Out]]),
    finalOutputs: Map[State, Set[Seq[Out]]]) extends Fst(states, Set(initial), finals) {

  /** The value of p */
  def p =
    finalOutputs.map(_._2.size).max

  def this(states: Set[State], initial: State, finals: Set[State], transitions: Map[(State, In), State], outputs: Map[(State, In), Seq[Out]], finalOutputs: Map[State, Set[Seq[Out]]]) =
    this(states, initial, finals, (transitions, outputs), finalOutputs)

  def this(states: Set[State], initial: State, finals: Set[State], transitions: Set[Transition[In, Out]], finalOutputs: Map[State, Set[Seq[Out]]]) =
    this(states, initial, finals, transitions.foldLeft(
      (Map.empty[(State, In), State],
        Map.empty[(State, In), Seq[Out]])) {
        case ((transAcc, outAcc), (origin, input, output, target)) =>
          val key = (origin, input)
          val transAcc1 =
            if (transAcc.contains(key))
              throw new Exception("Non deterministic Fst is not p-subsequential")
            else
              transAcc.updated(key, target)
          val outAcc1 = outAcc.updated(key, output)
          (transAcc1, outAcc1)
      }, finalOutputs)

  private val (transitionMap, outputMap) = maps

  /** Returns the next state when reading `in` in state `state`.
   *  If no transition exists, returns `None`.
   */
  def delta(state: State, in: In): Option[State] =
    transitionMap.get((state, in))

  /** Returns the state reached when reading word `ins` from state `state`.
   *  If at some point no transition can be found, returns `None`.
   */
  def delta(state: State, ins: Seq[In]): Option[State] =
    if (ins.isEmpty)
      Some(state)
    else
      delta(state, ins.head).flatMap(s => delta(s, ins.tail))

  /** Returns the output sequence encountered when reading `in` in state `state`. */
  def sigma(origin: State, in: In): Seq[Out] =
    outputMap.getOrElse((origin, in), Seq.empty)

  /** Returns the output sequence encountered when reading `ins` from state `state`. */
  def sigma(state: State, ins: Seq[In]): Seq[Out] =
    if (ins.isEmpty)
      Seq.empty[Out]
    else
      delta(state, ins.head) match {
        case Some(q) => sigma(state, ins.head) ++ sigma(q, ins.tail)
        case None    => Seq.empty[Out]
      }

  /** Returns the set of extra output associated to the state `state`. */
  def phi(state: State): Set[Seq[Out]] =
    finalOutputs.getOrElse(state, Set.empty)

  def toDot: String = {
    val trans = for {
      ((s1, in), s2) <- transitionMap
      out = outputMap.getOrElse((s1, in), Seq()).mkString
    } yield f"""q$s1->q$s2[label="$in:$out"]"""
    val out = for {
      (s1, out) <- finalOutputs
    } yield f"""q$s1->out[label="${out.map(_.mkString).mkString}"]"""
    toDot(trans ++ out)
  }

  /** pushes common prefixes in front when possible. */
  def push: PSubFst[In, Out] = {

    import scala.collection.{ mutable => mu }

    val prefixes = mu.Map.empty[State, Seq[Out]]

    val sigma2 = mu.Map.empty[(State, In), Seq[Out]] ++ outputMap

    val phi2 = mu.Map.empty[State, Set[Seq[Out]]] ++ finalOutputs

    def computePrefix(state: State, seen: Set[State]): Unit =
      if (!prefixes.contains(state) && !seen.contains(state)) {
        // first compute the prefixes of next states
        for (((`state`, _), q) <- transitionMap)
          computePrefix(q, seen + state)

        val outs =
          if (isFinal(state))
            phi(state)
          else
            Set.empty[Seq[Out]]

        val prefix =
          lcp(outs ++ (for (((`state`, i), q) <- transitionMap) yield sigma(state, i) ++ (if (q != state) prefixes(q) else Seq.empty)))

        prefixes(state) = prefix

        // push it in front of all incoming edges
        if (isFinal(state))
          phi2(state) = for (o <- phi(state)) yield o.drop(prefix.size)
        else if (isInitial(state))
          for (((`state`, i), o) <- outputMap) sigma2((state, i)) = (o ++ prefixes(transitionMap((state, i))))
        else
          for (((`state`, i), o) <- outputMap) sigma2((state, i)) = (o ++ prefixes(transitionMap((state, i)))).drop(prefix.size)
      }

    computePrefix(initial, Set.empty[State])

    new PSubFst(states, initial, finals, transitionMap, sigma2.toMap, phi2.toMap)

  }

}
