package lingua.lexikon

package object fst {

  type State = Int

  type Transition[In, Out] = (State, In, Seq[Out], State)

  def lcp[T](s1: Seq[T], s2: Seq[T]): Seq[T] =
    s1.zip(s2).takeWhile { case (t1, t2) => t1 == t2 }.unzip._1

  def lcp[T](ss: Iterable[Seq[T]]): Seq[T] =
    ss.foldLeft(None: Option[Seq[T]]) {
      case (None, s)      => Some(s)
      case (Some(s1), s2) => Some(lcp(s1, s2))
    }.getOrElse(Seq.empty[T])

}
