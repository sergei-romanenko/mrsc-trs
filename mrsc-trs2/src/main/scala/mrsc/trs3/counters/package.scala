package mrsc.trs3

package object counters {
  type Conf = List[Exp]
  type TransitionRule = PartialFunction[Conf, Conf]
  implicit def intToComponent(i: Int): Exp = Num(i)
  val ϖ = Omega
}