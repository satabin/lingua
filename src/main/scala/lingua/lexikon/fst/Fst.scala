package lingua
package lexikon
package fst

import scala.annotation.tailrec

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

class NFst[In, Out] private (states: Set[State], initials: Set[State], finals: Set[State], maps: (Map[(State, In), Set[State]], Map[(State, In, State), Seq[Out]])) extends Fst(states, initials, finals) {

  def this(states: Set[State], initials: Set[State], finals: Set[State], transitions: Map[(State, In), Set[State]], outputs: Map[(State, In, State), Seq[Out]]) =
    this(states, initials, finals, (transitions, outputs))

  def this(states: Set[State], initials: Set[State], finals: Set[State], transitions: Set[Transition[In, Out]]) =
    this(states, initials, finals, transitions.foldLeft(
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

    import scala.collection.{ mutable => mu }

    val f2 = mu.Set.empty[State]
    val output2 = new mu.HashMap[State, mu.Set[Seq[Out]]] with mu.MultiMap[State, Seq[Out]]

    val delta2 = mu.Map.empty[(State, In), State]
    val sigma2 = mu.Map.empty[(State, In), Seq[Out]]

    val queue = mu.Queue.empty[(State, Set[(State, Seq[Out])])]

    val initial = initials.map((_, Seq.empty[Out]))

    queue.enqueue((0, initial))

    val newStates = mu.Map[Set[State], State](initial.map(_._1) -> 0)
    var nextStateId = 1

    while (queue.nonEmpty) {

      val (q2id, q2) = queue.dequeue

      def j1(a: In) =
        for {
          (st, b) <- transitionMap.keys
          if a == b
          (_, w) <- q2.find(_._1 == st)
        } yield (st, w)

      def j2(a: In) =
        for {
          (st, b) <- transitionMap.keys
          if a == b
          (_, w) <- q2.find(_._1 == st).toSet
          st1 <- delta(st, a)
        } yield (st, w, st1)

      for {
        (q, w) <- q2
        if this.finals.contains(q)
      } {
        f2 + q2id
        output2.addBinding(q2id, w)
      }

      for {
        (q, w) <- q2
        (q_, a) <- transitionMap.keys
        if q == q_
      } {

        sigma2((q2id, a)) = lcp(
          for ((q, w) <- j1(a))
            yield w ++ lcp(
            for (q_ <- delta(q, a))
              yield sigma(q, a, q_)))

        val nextState =
          (for ((q, w, q_) <- j2(a))
            yield (q_, (w ++ sigma(q, a, q_)).drop(sigma2((q2id, a)).size))).toSet

        val nextId = newStates.get(nextState.map(_._1)) match {
          case Some(id) =>
            id
          case None =>
            newStates += (nextState.map(_._1) -> nextStateId)
            queue.enqueue((nextStateId, nextState))
            nextStateId += 1
            nextStateId - 1
        }

        delta2((q2id, a)) = nextId
      }

    }

    new PSubFst(newStates.values.toSet, 0, f2.toSet, delta2.toMap, sigma2.toMap, output2.mapValues(_.toSet).toMap)
  }

  def toDot: String = {
    val trans = for {
      ((s1, in), ss2) <- transitionMap
      s2 <- ss2
      out = outputMap.getOrElse((s1, in, s2), Seq()).mkString
    } yield f"""q$s1->q$s2[label="$in:$out"]"""
    toDot(trans)
  }

}

class PSubFst[In, Out] private (states: Set[State],
    initial: State,
    finals: Set[State],
    maps: (Map[(State, In), State], Map[(State, In), Seq[Out]]),
    finalOutputs: Map[State, Set[Seq[Out]]]) extends Fst(states, Set(initial), finals) {

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

  def delta(state: State, in: In): Option[State] =
    transitionMap.get((state, in))

  def delta(state: State, ins: Seq[In]): Option[State] =
    if (ins.isEmpty)
      Some(state)
    else
      delta(state, ins.head).flatMap(s => delta(s, ins.tail))

  def sigma(origin: State, in: In): Seq[Out] =
    outputMap.getOrElse((origin, in), Seq.empty)

  def sigma(state: State, ins: Seq[In]): Seq[Out] =
    if (ins.isEmpty)
      Seq.empty[Out]
    else
      delta(state, ins.head) match {
        case Some(q) => sigma(state, ins.head) ++ sigma(q, ins.tail)
        case None    => Seq.empty[Out]
      }

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

}
