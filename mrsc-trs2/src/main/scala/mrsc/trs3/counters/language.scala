package mrsc.trs3.counters

import collection.mutable.ListBuffer
import mrsc.trs3._

sealed trait Exp {
  def +(comp: Exp): Exp
  def -(comp: Exp): Exp
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Num(i: Int) extends Exp {
  override def +(comp: Exp) = comp match {
    case Omega => Omega
    case Num(j) => Num(i + j)
  }
  override def -(comp: Exp) = comp match {
    case Omega => Omega
    case Num(j) => Num(i - j)
  }
  override def ===(j: Int) = i == j
  override def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Exp {
  def +(comp: Exp) = Omega
  def -(comp: Exp) = Omega
  def >=(comp: Int) = true
  override def ===(j: Int) = true
  override def toString = "ϖ"
}

trait CountersSyntax extends TRSSyntax[Conf] {
  def equiv(c1: Conf, c2: Conf) = CountersSyntax.equiv(c1, c2)
  def instanceOf(c1: Conf, c2: Conf) = CountersSyntax.instanceOf(c1, c2)
  def rebuildings(c: Conf) = CountersSyntax.rebuildings(c)
}

object CountersSyntax extends {
  def equiv(c1: Conf, c2: Conf) = c1 == c2

  def instanceOf(c1: Conf, c2: Conf): Boolean =
    (c1, c2).zipped.forall(instanceOf)

  def instanceOf(x: Exp, y: Exp) = (x, y) match {
    case (_, Omega) => true
    case (_, _) => x == y
  }

  private def cartProd[T](zzs: List[List[T]]): List[List[T]] = zzs match {
    case Nil => List(List())
    case xs :: xss => for (y <- xs; ys <- cartProd(xss)) yield y :: ys
  }

  def rebuildings(c: Conf) = cartProd(c map genComp) - c

  private def genComp(c: Exp): List[Exp] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

case class Rule[C](guard: Boolean, next: C)

class Rules[C] {
  val buffer = new ListBuffer[Rule[C]]

  def fromTo(guard: Boolean, next: C) {
    buffer += Rule(guard, next)
  }

  def rules: List[Rule[C]] = buffer.result()

  implicit def booleanToGuardHelper[C](guard: Boolean) =
    new GuardHelper(guard, this)
}

class GuardHelper[C](guard: Boolean, builder: Rules[C]) {
  def -->(next: C) = builder.fromTo(guard, next)
}

/*
trait CountersSemantics extends RewriteSemantics[Conf] {
  val protocol: Protocol
  def driveConf(c: Conf) = protocol.rules.map { _.lift(c) }
}
*/

trait CountersSemantics extends RewriteSemantics[Conf] {
  val protocol: Protocol
  def driveConf(c: Conf): List[Option[Conf]] = {
    protocol match {
      case protocol3: Protocol3 => {
        for (rule <- protocol3.tr(c).rules)
          yield (if (rule.guard) Some(rule.next) else None)
      }
      case _ => protocol.rules.map { _.lift(c) }
    }
  }
}

trait LWhistle {
  val l: Int
  def dangerous(counter: Conf) = counter exists {
    case Num(i) => i >= l
    case Omega => false
  }
}
