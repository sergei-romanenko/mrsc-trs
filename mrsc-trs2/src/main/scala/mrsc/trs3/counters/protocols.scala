package mrsc.trs3.counters

trait Protocol {
  val start: Conf
  val tr: Tr
  def unsafe(c: Conf): Boolean
}

case object Synapse extends Protocol {
  val start: Conf = List(Omega, 0, 0)

  val tr: Tr = {
    case List(i, d, v) => new Rules[Conf] {
      (i >= 1) -->
        List(i + d - 1, 0, v + 1)
      (v >= 1) -->
        List(i + d + v - 1, 1, 0)
      (i >= 1) -->
        List(i + d + v - 1, 1, 0)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, d, v) if d >= 1 && v >= 1 => true
    case List(i, d, v) if d >= 2 => true
    case _ => false
  }
}

// invalid, modified, shared
case object MSI extends Protocol {
  val start: Conf = List(Omega, 0, 0)
  val tr: Tr = {
    case List(i, m, s) => new Rules[Conf] {
      (i >= 1) -->
        List(i + m + s - 1, 1, 0)
      (i >= 1) -->
        List(i, m, s)
      (i >= 1) -->
        List(i - 1, 0, m + s + 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, m, s) if m >= 1 && s >= 1 => true
    case List(i, m, s) if m >= 2 => true
    case _ => false
  }
}

// invalid, modified, shared, owned
case object MOSI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val tr: Tr = {
    case List(i, o, s, m) => new Rules[Conf] {
      (i >= 1) -->
        List(i - 1, m + o, s + 1, 0)
      (o >= 1) -->
        List(i + o + s + m - 1, 0, 0, 1)
      // wI
      (i >= 1) -->
        List(i + o + s + m - 1, 0, 0, 1)
      // wS
      (s >= 1) -->
        List(i + o + s + m - 1, 0, 0, 1)
      // se
      (s >= 1) -->
        List(i + 1, o, s - 1, m)
      // wbm
      (m >= 1) -->
        List(i + 1, o, s, m - 1)
      // wbo
      (o >= 1) -->
        List(i + 1, o - 1, s, m)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, o, s, m) if o >= 2 => true
    case List(i, o, s, m) if m >= 2 => true
    case List(i, o, s, m) if s >= 1 && m >= 1 => true
    case _ => false
  }
}

case object MESI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val tr: Tr = {
    case List(i, e, s, m) => new Rules[Conf] {
      (i >= 1) -->
        List(i - 1, 0, s + e + m + 1, 0)
      (e >= 1) -->
        List(i, e - 1, s, m + 1)
      (s >= 1) -->
        List(i + e + s + m - 1, 1, 0, 0)
      (i >= 1) -->
        List(i + e + s + m - 1, 1, 0, 0)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, e, s, m) if m >= 2 => true
    case List(i, e, s, m) if s >= 1 && m >= 1 => true
    case _ => false
  }
}

case object MOESI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0)
  val tr: Tr = {
    case List(i, m, s, e, o) => new Rules[Conf] {
      // rm
      (i >= 1) -->
        List(i - 1, 0, s + e + 1, 0, o + m)
      //wh2
      (e >= 1) -->
        List(i, m + 1, s, e - 1, o)
      // wh3
      (s + o >= 1) -->
        List(i + m + s + e + o - 1, 0, 0, 1, 0)
      // wm
      (i >= 1) -->
        List(i + m + s + e + o - 1, 0, 0, 1, 0)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, m, s, e, o) if m >= 1 && (e + s + o) >= 1 => true
    case List(i, m, s, e, o) if m >= 2 => true
    case List(i, m, s, e, o) if e >= 2 => true
    case _ => false
  }
}

case object Illinois extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val tr: Tr = {
    case List(i, e, d, s) => new Rules[Conf] {
      // r2
      (i >= 1 && e === 0 && d === 0 && s === 0) -->
        List(i - 1, 1, 0, 0)
      // r3
      (i >= 1 && d >= 1) -->
        List(i - 1, e, d - 1, s + 2)
      // r4
      (i >= 1 && s + e >= 1) -->
        List(i - 1, 0, d, s + e + 1)
      // r6
      (e >= 1) -->
        List(i, e - 1, d + 1, s)
      // r7
      (s >= 1) -->
        List(i + s - 1, e, d + 1, 0)
      // r8
      (i >= 1) -->
        List(i + e + d + s - 1, 0, 1, 0)
      // r9
      (d >= 1) -->
        List(i + 1, e, d - 1, s)
      // r10
      (s >= 1) -->
        List(i + 1, e, d, s - 1)
      // r11
      (e >= 1) -->
        List(i + 1, e - 1, d, s)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, e, d, s) if d >= 1 && s >= 1 => true
    case List(i, e, d, s) if d >= 2 => true
    case _ => false
  }
}

case object Berkley extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val tr: Tr = {
    case List(i, n, u, e) => new Rules[Conf] {
      // rm
      (i >= 1) -->
        List(i - 1, n + e, u + 1, 0)
      // wm 
      (i >= 1) -->
        List(i + n + u + e - 1, 0, 0, 1)
      // wh1 
      (n + u >= 1) -->
        List(i + n + u - 1, 0, 0, e + 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, n, u, e) if e >= 1 && u + n >= 1 => true
    case List(i, n, u, e) if e >= 2 => true
    case _ => false
  }
}

case object Firefly extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val tr: Tr = {
    case List(i, e, s, d) => new Rules[Conf] {
      // rm1
      (i >= 1 && d === 0 && s === 0 && e === 0) -->
        List(i - 1, 1, 0, 0)
      // rm2
      (i >= 1 && d >= 1) -->
        List(i - 1, e, s + 2, d - 1)
      // rm3
      (i >= 1 && s + e >= 1) -->
        List(i - 1, 0, s + e + 1, d)
      // wh2
      (e >= 1) -->
        List(i, e - 1, s, d + 1)
      // wh3
      (s === 1) -->
        List(i, e + 1, 0, d)
      // wm
      (i >= 1) -->
        List(i + e + d + s - 1, 0, 0, 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, e, s, d) if d >= 1 && s + e >= 1 => true
    case List(i, e, s, d) if e >= 2 => true
    case List(i, e, s, d) if d >= 2 => true
    case _ => false
  }
}

case object Futurebus extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0, 0, 0, 0, 0)
  //val start: Conf = List(ϖ, 0, 0, 0, ϖ, 0, 0, 0, ϖ)
  val tr: Tr = {
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) => new Rules[Conf] {
      // r2
      (i >= 1 && pW === 0) -->
        List(i - 1, 0, 0, 0, pR + 1, pW, pEMR + eM, pEMW, pSU + sU + eU)
      // r3
      (pEMR >= 1) -->
        List(i, sU + pR + 1, eU, eM, 0, pW, pEMR - 1, pEMW, pSU)
      // r4
      (pSU >= 1) -->
        List(i, sU + pR + pSU, eU, eM, 0, pW, pEMR, pEMW, 0)
      // r5
      (pR >= 2 && pSU === 0 && pEMR === 0) -->
        List(i, sU + pR, eU, eM, 0, pW, 0, pEMW, 0)
      // r6
      (pR === 1 && pSU === 0 && pEMR === 0) -->
        List(i, sU, eU + 1, eM, 0, pW, 0, pEMW, 0)
      // wm1
      (i >= 1 & pW === 0) -->
        List(i + eU + sU + pSU + pR + pEMR - 1, 0, 0, 0, 0, 1, 0, pEMW + eM, 0)
      // wm2
      (pEMW >= 1) -->
        List(i + 1, sU, eU, eM + pW, pR, 0, pEMR, pEMW - 1, pSU)
      // wm3
      (pEMW === 0) -->
        List(i, sU, eU, eM + pW, pR, 0, pEMR, 0, pSU)
      // wh2
      (eU >= 1) -->
        List(i, sU, eU - 1, eM + 1, pR, pW, pEMR, pEMW, pSU)
      // wh2
      (sU >= 1) -->
        List(i + sU - 1, 0, eU, eM + 1, pR, pW, pEMR, pEMW, pSU)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 && eU + eM >= 1 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU + eM >= 2 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 1 && pW >= 1 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pW >= 2 => true
    case _ => false
  }
}

//invalid ≥ 1, dirty = 0, shared_clean = 0, shared_dirty = 0, exclusive = 0 —>
case object Xerox extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0)
  val tr: Tr = {
    case List(i, sc, sd, d, e) => new Rules[Conf] {
      // (1) rm1
      (i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0) -->
        List(i - 1, 0, 0, 0, 1)
      // (2) rm2
      (i >= 1 && d + sc + e + sd >= 1) -->
        List(i - 1, sc + e + 1, sd + d, 0, 0)
      // (3) wm1
      (i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0) -->
        List(i - 1, 0, 0, 1, 0)
      // (4) wm2
      (i >= 1 && d + sc + e + sd >= 1) -->
        List(i - 1, sc + e + 1 + sd + d, sd, 0, 0)
      // (5) wh1
      (d >= 1) -->
        List(i + 1, d - 1, sc, sd, d, e)
      // (6) wh2
      (sc >= 1) -->
        List(i + 1, sc - 1, sd, d, e)
      // (7) wh3
      (sd >= 1) -->
        List(i + 1, sc, sd - 1, d, e)
      // (8) wh4
      (e >= 1) -->
        List(i + 1, sc, sd, d, e - 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(i, sc, sd, d, e) if d >= 1 && (e + sc + sd) >= 1 => true
    case List(i, sc, sd, d, e) if e >= 1 && (sc + sd) >= 1 => true
    case List(i, sc, sd, d, e) if d >= 2 => true
    case List(i, sc, sd, d, e) if e >= 2 => true
    case _ => false
  }
}

case object Java extends Protocol {
  // nb 1 = True, nb 0 = False
  // race = 0 = H0, -1 = H1 ,,,
  val start: Conf = List(1, 0, Omega, 0, 0, 0, 0, 0)
  val tr: Tr = {
    case List(nb, race, i, b, o, in, out, w) => new Rules[Conf] {
      // (get fast)
      (nb === 1 && i >= 1) -->
        List(0, race, i - 1, 0, o + 1, in, out, w)
      // (put fast)
      (nb === 0 && b === 0 && o >= 1) -->
        List(1, race, i + 1, b, o - 1, in, out, w)
      // (get slow)
      (nb === 0 && i >= 1) -->
        List(nb, race, i - 1, b + 1, o, in + 1, out, w)
      // (put slow)
      (nb === 0 && b >= 1 && o >= 1) -->
        List(nb, race, i, b - 1, o - 1, in, out + 1, w)
      // (request)
      (race === 0 && in >= 1) -->
        List(nb, -1, i, b, o, in - 1, out, w + 1)
      // (request)
      (race === -2 && in >= 1) -->
        List(nb, -3, i, b, o, in - 1, out, w + 1)
      // (release)
      (race === 0 && out >= 1) -->
        List(nb, -2, i + 1, b, o, in, out - 1, w)
      // (release)
      (race === -1 && out >= 1) -->
        List(nb, -3, i + 1, b, o, in, out - 1, w)
      // (go)
      (race === -3 && w >= 1) -->
        List(nb, race, i, b, o + 1, in, out, w - 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(nb, race, i, b, o, in, out, w) if o + out >= 2 => true
    case _ => false
  }
}

case object ReaderWriter extends Protocol {
  val start: Conf = List(1, 0, 0, Omega, 0, 0)
  val tr: Tr = {
    case List(x2, x3, x4, x5, x6, x7) => new Rules[Conf] {
      // r1
      (x2 >= 1 && x4 === 0 && x7 >= 1) -->
        List(x2 - 1, x3 + 1, 0, x5, x6, x7)
      // r2
      (x2 >= 1 && x6 >= 1) -->
        List(x2, x3, x4 + 1, x5, x6 - 1, x7)
      // r3
      (x3 >= 1) -->
        List(x2 + 1, x3 - 1, x4, x5 + 1, x6, x7)
      // r4
      (x4 >= 1) -->
        List(x2, x3, x4 - 1, x5 + 1, x6, x7)
      // r5
      (x5 >= 1) -->
        List(x2, x3, x4, x5 - 1, x6 + 1, x7)
      // r6
      (x5 >= 1) -->
        List(x2, x3, x4, x5 - 1, x6, x7 + 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(x2, x3, x4, x5, x6, x7) if x3 >= 1 && x4 >= 1 => true
    case _ => false
  }
}

case object DataRace extends Protocol {
  val start: Conf = List(Omega, 0, 0)
  val tr: Tr = {
    case List(out, cs, scs) => new Rules[Conf] {
      // 1
      (out >= 1 && cs === 0 && scs === 0) -->
        List(out - 1, 1, 0)
      // 2
      (out >= 1 && cs === 0) -->
        List(out - 1, 0, scs + 1)
      // 3
      (cs >= 1) -->
        List(out + 1, cs - 1, scs)
      // 4
      (scs >= 1) -->
        List(out + 1, cs, scs - 1)
    }
  }

  def unsafe(c: Conf) = c match {
    case List(out, cs, scs) if cs >= 1 && scs >= 1 => true
    case _ => false
  }
}