package mrsc.trs2

package object counters {
  type Conf = List[Exp]
  type TransitionRule = PartialFunction[Conf, Conf]
  implicit def intToExp(i: Int): Exp = Num(i)
  val ϖ = Omega
}