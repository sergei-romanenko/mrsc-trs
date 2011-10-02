package mrsc.trs3

package object counters {
  type Conf = List[Exp]
  type TransitionRule = PartialFunction[Conf, Conf]
  type Tr = PartialFunction[Conf, Rules[Conf]]
  implicit def intToExp(i: Int): Exp = Num(i)
  val ϖ = Omega
}