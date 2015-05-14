package lingua.lexikon

package object fst {

  type State = Int

  type Transition[In, Out] = (State, In, Seq[Out], State)

  def lcp[T](s1: Seq[T], s2: Seq[T]): Seq[T] =
    s1.zip(s2).takeWhile { case (t1, t2) => t1 == t2 }.unzip._1

  def lcp[T](ss: Set[Seq[T]]): Seq[T] =
    if (ss.isEmpty) {
      Seq.empty
    } else if (ss.size == 1) {
      ss.head
    } else {
      val s1 = ss.head
      val ss1 = ss - s1
      if (s1.isEmpty) {
        s1
      } else {
        val s2 = ss1.head
        lcp(ss1 + lcp(s1, s2) - s2)
      }
    }

}
