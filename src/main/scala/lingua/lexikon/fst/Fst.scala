package lingua
package lexikon
package fst

abstract class Fst[In, Out](val initials: Set[State], val finals: Set[State]) {

  def isFinal(state: State): Boolean =
    finals.contains(state)

  def isInitial(state: State): Boolean =
    initials.contains(state)

}

class NFst[In, Out] private (initials: Set[State], finals: Set[State], maps: (Map[(State, In), Set[State]], Map[(State, In, State), Seq[Out]])) extends Fst(initials, finals) {

  def this(initials: Set[State], finals: Set[State], transitions: Map[(State, In), Set[State]], outputs: Map[(State, In, State), Seq[Out]]) =
    this(initials, finals, (transitions, outputs))

  def this(initials: Set[State], finals: Set[State], transitions: Set[Transition[In, Out]]) =
    this(initials, finals, transitions.foldLeft(
      (Map.empty[(State, In), Set[State]],
        Map.empty[(State, In, State), Seq[Out]])) {
        case ((transAcc, outAcc), (origin, input, output, target)) =>
          val transKey = (origin, input)
          val outKey = (origin, input, target)
          val transAcc1 =
            if (transAcc.contains(transKey))
              transAcc.updated(transKey, transAcc(transKey) + target)
            else
              transAcc.updated(transKey, Set(target))
          val outAcc1 = outAcc.updated(outKey, output)
          (transAcc1, outAcc1)
      })

  private val (transitionMap, outputMap) = maps

  def delta(state: State, in: In): Set[State] =
    transitionMap.getOrElse((state, in), Set.empty)

  def sigma(origin: State, in: In, target: State): Seq[Out] =
    outputMap.getOrElse((origin, in, target), Seq.empty)

  def determinize: PSubFst[In, Out] = {
    def loop(queue: Set[(State, Set[(State, Seq[Out])])], nextState: State, finalsAcc: Set[State], transitionsAcc: Map[(State, In), State], outputAcc: Map[(State, In), Seq[Out]], finalOutputsAcc: Map[State, Set[Seq[Out]]]) =
      if (queue.isEmpty) {
        new PSubFst(0, finalsAcc, transitionsAcc, outputAcc, finalOutputsAcc)
      } else {
        val (stateId, states) = queue.head
        val (finalsAcc1, finalOutputsAcc1) =
          states.foldLeft((finalsAcc, finalOutputsAcc)) {
            case ((finalsAcc, finalOutputsAcc), (st1, out1)) if finals.contains(st1) =>
              val finalOutputsAcc1 =
                finalOutputsAcc.updated(stateId, finalOutputsAcc.getOrElse(stateId, Set.empty) + out1)
              (finalsAcc + stateId, finalOutputsAcc1)
            case (acc, _) =>
              acc
          }
        val newTransitions = for {
          (st, a) <- transitionMap.keys
          (_, w) <- states.find(_._1 == st)
        } yield (st, a, w)
        val (queue1, nextState1, transitionsAcc1, outputAcc1) =
          newTransitions.foldLeft((queue, nextState, transitionsAcc, outputAcc)) {
            case ((queue, nextState, transitionsAcc, outputAcc), (q, a, w)) =>
              val qs = delta(q, a)
              val outputAcc1 =
                outputAcc.updated((stateId, a), lcp(w, lcp(qs.map(sigma(q, a, _)))))
              val transitionsAcc1 =
                transitionsAcc.updated((stateId, a), nextState)
              val queue1 = ???
              val nextState1 = nextState + 1
              (queue1, nextState1, transitionsAcc1, outputAcc1)
          }
        ???
      }
    val initial = (0, initials.map((_, Seq.empty[Out])))
    loop(Set(initial), 1, Set.empty, Map.empty, Map.empty, Map.empty)
  }

}

class PSubFst[In, Out] private (initial: State,
    finals: Set[State],
    maps: (Map[(State, In), State], Map[(State, In), Seq[Out]]),
    finalOutputs: Map[State, Set[Seq[Out]]]) extends Fst(Set(initial), finals) {

  def this(initial: State, finals: Set[State], transitions: Map[(State, In), State], outputs: Map[(State, In), Seq[Out]], finalOutputs: Map[State, Set[Seq[Out]]]) =
    this(initial, finals, (transitions, outputs), finalOutputs)

  def this(initial: State, finals: Set[State], transitions: Set[Transition[In, Out]], finalOutputs: Map[State, Set[Seq[Out]]]) =
    this(initial, finals, transitions.foldLeft(
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

  def delta(state: State, in: In): Option[State] =
    transitionMap.get((state, in))

  def sigma(origin: State, in: In): Seq[Out] =
    outputMap.getOrElse((origin, in), Seq.empty)

  def phi(state: State): Set[Seq[Out]] =
    finalOutputs.getOrElse(state, Set.empty)

}
