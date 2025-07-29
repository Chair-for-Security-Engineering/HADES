import spinal.core._
import spinal.core.internals._

abstract class AESSbox extends Template {

  val io = new Bundle {
    val sboxIn        = in  Bits(8 bits)
    val isInverseSbox = in  Bool()
    val sboxOut       = out Bits(8 bits)
  }

  io.isInverseSbox.addTag(publicInput)

  override def instantiate(): Boolean = false
}

abstract class AESSboxFactory extends TemplateFactory {
  type T <: AESSbox

  override def createTemplate(): T = ???
  def createTemplate(pipelined : Boolean): AESSbox
  def getName(): String
}

case class CanrightSboxFactory() extends AESSboxFactory {
  type T = CanrightSbox

  override def createTemplate(): T = CanrightSbox(pipelined = false)
  override def createTemplate(pipelined : Boolean): T = CanrightSbox(pipelined = pipelined)
  override def getName(): String = "CanrightSbox"
}

case class BoyarPeraltaSboxFactory() extends AESSboxFactory {
  type T = BoyarPeraltaSbox

  override def createTemplate(): T = BoyarPeraltaSbox(pipelined = false)
  override def createTemplate(pipelined : Boolean): T = BoyarPeraltaSbox(pipelined = pipelined)
  override def getName(): String = "Depth16Sbox"
}

object AllSboxFactories extends Configurable {
  parameters = List(
    CanrightSboxFactory(),
    BoyarPeraltaSboxFactory()
  )
}

////////////////////////////////////////////////////////////////////////
// Helper templates for GF(2^n) arithmetic needed for the Canright Sbox
////////////////////////////////////////////////////////////////////////
// We skip Canright's optimization steps which replace XOR and AND gates
// by a single OR gate, since no OR gadgets are currently included.

// Square / Invert in GF(2^2) using normal basis [Omega^2, Omega]
case class GF_SQ_2(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a = in  Bits(2 bits)
    val q = out Bits(2 bits)
  }

  override def instantiate(): Boolean = {
    io.q(1) := io.a(0)
    io.q(0) := io.a(1)

    this.latency = 0
    true
  }
}

// Scale by w = Omega in GF(2^2) using normal basis [Omega^2, Omega]
case class GF_SCLW_2(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a = in  Bits(2 bits)
    val q = out Bits(2 bits)
  }

  override def instantiate(): Boolean = {
    io.q(1) := Xor(io.a(1), io.a(0))
    val delay1 = if (pipelined) latencyXor else 0
    io.q(0) := Delay(io.a(1), delay1)

    this.latency = latencyXor
    true
  }
}

// Scale by w^2 = Omega^2 in GF(2^2) using normal basis [Omega^2, Omega]
case class GF_SCLW2_2(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a = in  Bits(2 bits)
    val q = out Bits(2 bits)
  }

  override def instantiate(): Boolean = {
    val delay1 = if (pipelined) latencyXor else 0
    io.q(1) := Delay(io.a(0), delay1)
    io.q(0) := Xor(io.a(1), io.a(0))

    this.latency = latencyXor
    true
  }
}

// Multiply in GF(2^2), shared factors, using normal basis [Omega^2, Omega]
case class GF_MULS_2(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a  = in  Bits(2 bits)
    val ab = in  Bool()
    val b  = in  Bits(2 bits)
    val cd = in  Bool()
    val q  = out Bits(2 bits)
  }

  override def instantiate(): Boolean = {
    val abcd = Bool()
    val p = Bool()
    val q = Bool()

    abcd := Not(And(io.ab, io.cd))
    p := Xor(Not(And(io.a(1), io.b(1))), abcd)
    q := Xor(Not(And(io.a(0), io.b(0))), abcd)
    io.q(1) := p
    io.q(0) := q

    this.latency = latencyNot + latencyAnd + latencyXor
    true
  }
}

// Multiply & Scale by N in GF(2^2), shared factors, basis [Omega^2, Omega]
case class GF_MULS_SCL_2(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a  = in  Bits(2 bits)
    val ab = in  Bool()
    val b  = in  Bits(2 bits)
    val cd = in  Bool()
    val q  = out Bits(2 bits)
  }

  override def instantiate(): Boolean = {
    val t = Bool()
    val p = Bool()
    val q = Bool()


    t := Not(And(io.a(0), io.b(0)))
    p := Xor(Not(And(io.ab, io.cd)), t)
    q := Xor(Not(And(io.a(1), io.b(1))), t)
    io.q(1) := p
    io.q(0) := q

    this.latency = latencyNot + latencyAnd + latencyXor
    true
  }
}

// Inverse in GF(2^4)/GF(2^2) using normal basis [alpha^8, alpha^2]
case class GF_INV_4(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a = in  Bits(4 bits)
    val q = out Bits(4 bits)
  }

  override def instantiate(): Boolean = {
    val a, b, c, d, p, q, ab, ab2, ab2N = Bits(2 bits)
    val sa, sb, sd = Bool()

    val abmul = GF_MULS_2(pipelined)
    val absq = GF_SQ_2(pipelined)
    val absclN = GF_SCLW2_2(pipelined)
    val dinv = GF_SQ_2(pipelined)
    val pmul = GF_MULS_2(pipelined)
    val qmul = GF_MULS_2(pipelined)
    absq.instantiateTemplate()
    abmul.instantiateTemplate()
    absclN.instantiateTemplate()
    dinv.instantiateTemplate()
    pmul.instantiateTemplate()
    qmul.instantiateTemplate()
    absq.control := control
    abmul.control := control
    absclN.control := control
    dinv.control := control
    pmul.control := control
    qmul.control := control

    a := io.a(3 downto 2)
    b := io.a(1 downto 0)
    sa := Xor(a(1), a(0))
    sb := Xor(b(1), b(0))

    // GF_MULS_2 abmul(a, sa, b, sb, ab);
    abmul.io.a := a
    abmul.io.ab := sa
    abmul.io.b := b
    abmul.io.cd := sb
    ab := abmul.io.q

    // GF_SQ_2 absq( (a ^ b), ab2);
    for (i <- 1 downto 0) {
      absq.io.a(i) := Xor(a(i), b(i))
    }
    ab2 := absq.io.q

    // GF_SCLW2_2 absclN( ab2, ab2N);
    absclN.io.a := ab2
    ab2N := absclN.io.q

    // GF_SQ_2 dinv( (ab ^ ab2N), d);
    for (i <- 1 downto 0) {
      val delay1 = if (pipelined) latencyAnd + latencyNot else 0
      dinv.io.a(i) := Xor(ab(i), Delay(ab2N(i), delay1))
    }
    d := dinv.io.q

    sd := Xor(d(1), d(0))

    // GF_MULS_2 pmul(d, sd, b, sb, p);
    pmul.io.a := d
    pmul.io.ab := sd
    val delay2 = if (pipelined) dinv.latency + latencyXor + abmul.latency + latencyXor else 0
    for (i <- 1 downto 0) {
      pmul.io.b(i) := Delay(b(i), delay2)
    }
    pmul.io.cd := Delay(sb, delay2)
    p := pmul.io.q

    // GF_MULS_2 qmul(d, sd, a, sa, q);
    qmul.io.a := d
    qmul.io.ab := sd
    for (i <- 1 downto 0) {
      qmul.io.b(i) := Delay(a(i), delay2)
    }
    qmul.io.cd := Delay(sa, delay2)
    q := qmul.io.q

    io.q(3 downto 2) := p
    io.q(1 downto 0) := q

    this.latency = pmul.latency + latencyXor + dinv.latency + scala.math.max(abmul.latency, absclN.latency + absq.latency)
    true
  }
}

// Square & scale by nu in GF(2^4)/GF(2^2) using normal basis [alpha^8, alpha^2]
// nu = beta^8 = N^2*alpha^2, N = w^2
case class GF_SQ_SCL_4(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val a = in  Bits(4 bits)
    val q = out Bits(4 bits)
  }

  override def instantiate(): Boolean = {
    val a, b, ab2, b2, b2N2 = Bits(2 bits)

    val absq = GF_SQ_2(pipelined)
    val bsq = GF_SQ_2(pipelined)
    val bmulN2 = GF_SCLW_2(pipelined)
    absq.instantiateTemplate()
    bsq.instantiateTemplate()
    bmulN2.instantiateTemplate()
    absq.control := control
    bsq.control := control
    bmulN2.control := control

    a := io.a(3 downto 2)
    b := io.a(1 downto 0)

    // GF_SQ_2 absq(a ^ b,ab2);
    for (i <- 1 downto 0) {
      absq.io.a(i) := Xor(a(i), b(i))
    }
    ab2 := absq.io.q

    // GF_SQ_2 bsq(b,b2);
    bsq.io.a := b
    b2 := bsq.io.q

    // GF_SCLW_2 bmulN2(b2,b2N2);
    bmulN2.io.a := b2
    b2N2 := bmulN2.io.q

    io.q(3 downto 2) := ab2
    io.q(1 downto 0) := b2N2

    this.latency = scala.math.max(latencyXor + absq.latency, bmulN2.latency + bsq.latency)
    true
  }
}

// Multiply in GF(2^4)/GF(2^2), shared factors, basis [alpha^8, alpha^2]
case class GF_MULS_4(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val A  = in  Bits(4 bits)
    val a  = in  Bits(2 bits)
    val Al = in  Bool()
    val Ah = in  Bool()
    val aa = in  Bool()
    val B  = in  Bits(4 bits)
    val b  = in  Bits(2 bits)
    val Bl = in  Bool()
    val Bh = in  Bool()
    val bb = in  Bool()
    val Q  = out Bits(4 bits)
  }

  override def instantiate(): Boolean = {
    val ph, pl, ps, p = Bits(2 bits)
    val t = Bool()

    val himul = GF_MULS_2(pipelined)
    val lomul = GF_MULS_2(pipelined)
    val summul = GF_MULS_SCL_2(pipelined)
    himul.instantiateTemplate()
    lomul.instantiateTemplate()
    summul.instantiateTemplate()
    himul.control := control
    lomul.control := control
    summul.control := control

    // GF_MULS_2 himul(A[3:2], Ah, B[3:2], Bh, ph);
    himul.io.a := io.A(3 downto 2)
    himul.io.ab := io.Ah
    himul.io.b := io.B(3 downto 2)
    himul.io.cd := io.Bh
    ph := himul.io.q

    // GF_MULS_2 lomul(A[1:0], Al, B[1:0], Bl, pl);
    lomul.io.a := io.A(1 downto 0)
    lomul.io.ab := io.Al
    lomul.io.b := io.B(1 downto 0)
    lomul.io.cd := io.Bl
    pl := lomul.io.q

    // GF_MULS_SCL_2 summul( a, aa, b, bb, p);
    summul.io.a := io.a
    summul.io.ab := io.aa
    summul.io.b := io.b
    summul.io.cd := io.bb
    p := summul.io.q

    io.Q(3) := Xor(ph(1), p(1))
    io.Q(2) := Xor(ph(0), p(0))
    io.Q(1) := Xor(pl(1), p(1))
    io.Q(0) := Xor(pl(0), p(0))

    this.latency = latencyXor + scala.math.max(himul.latency, summul.latency)
    true
  }
}

// Inverse in GF(2^8)/GF(2^4), using normal basis [d^16, d]
case class GF_INV_8(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val A = in  Bits(8 bits)
    val Q = out Bits(8 bits)
  }

  override def instantiate(): Boolean = {
    val a, b, d, p, q = Bits(4 bits)
    val sa, sb, sd, t = Bits(2 bits)
    val al, ah, aa, bl, bh, bb, dl, dh, dd = Bool()

    val ab, ab2 = Bits(4 bits)

    val abmul = GF_MULS_4(pipelined)
    val absq = GF_SQ_SCL_4(pipelined)
    val dinv = GF_INV_4(pipelined)
    abmul.instantiateTemplate()
    absq.instantiateTemplate()
    dinv.instantiateTemplate()
    abmul.control := control
    absq.control := control
    dinv.control := control

    val pmul = GF_MULS_4(pipelined)
    val qmul = GF_MULS_4(pipelined)
    pmul.instantiateTemplate()
    qmul.instantiateTemplate()
    pmul.control := control
    qmul.control := control

    a := io.A(7 downto 4)
    b := io.A(3 downto 0)
    for (i <- 1 downto 0) {
      sa(i) := Xor(a(2 + i), a(i))
      sb(i) := Xor(b(2 + i), b(i))
    }
    al := Xor(a(1), a(0))
    ah := Xor(a(3), a(2))
    aa := Xor(sa(1), sa(0))
    bl := Xor(b(1), b(0))
    bh := Xor(b(3), b(2))
    bb := Xor(sb(1), sb(0))

    // GF_MULS_4 abmul(a, sa, al, ah, aa, b, sb, bl, bh, bb, ab);
    abmul.io.A := a
    abmul.io.a := sa
    abmul.io.Al := al
    abmul.io.Ah := ah
    abmul.io.aa := aa
    abmul.io.B := b
    abmul.io.b := sb
    abmul.io.Bl := bl
    abmul.io.Bh := bh
    abmul.io.bb := bb
    ab := abmul.io.Q

    // GF_SQ_SCL_4 absq( (a ^ b), ab2);
    for (i <- 3 downto 0) {
      absq.io.a(i) := Xor(a(i), b(i))
      // We delay the output so that it arrives at the same time as ab
      val delay1 = if (pipelined) latencyAnd + latencyNot else 0
      ab2(i) := Delay(absq.io.q(i), delay1)
    }

    // GF_INV_4 dinv( (ab ^ ab2), d);
    for (i <- 3 downto 0) {
      dinv.io.a(i) := Xor(ab(i), ab2(i))
    }
    d := dinv.io.q

    for (i <- 1 downto 0) {
      sd(i) := Xor(d(2 + i), d(i))
    }
    dl := Xor(d(1), d(0))
    dh := Xor(d(3), d(2))
    dd := Xor(sd(1), sd(0))

    // GF_MULS_4 pmul(d, sd, dl, dh, dd, b, sb, bl, bh, bb, p);
    pmul.io.A := d
    pmul.io.a := sd
    pmul.io.Al := dl
    pmul.io.Ah := dh
    pmul.io.aa := dd

    val delay2 = if (pipelined) dinv.latency + latencyXor + abmul.latency else 0
    for (i <- 3 downto 0) {
      pmul.io.B(i) := Delay(b(i), delay2)
    }
    for (i <- 1 downto 0) {
      pmul.io.b(i) := Delay(sb(i), delay2)
    }
    pmul.io.Bl := Delay(bl, delay2)
    pmul.io.Bh := Delay(bh, delay2)
    pmul.io.bb := Delay(bb, delay2)
    p := pmul.io.Q

    // GF_MULS_4 qmul(d, sd, dl, dh, dd, a, sa, al, ah, aa, q);
    qmul.io.A := d
    qmul.io.a := sd
    qmul.io.Al := dl
    qmul.io.Ah := dh
    qmul.io.aa := dd
    for (i <- 3 downto 0) {
      qmul.io.B(i) := Delay(a(i), delay2)
    }
    for (i <- 1 downto 0) {
      qmul.io.b(i) := Delay(sa(i), delay2)
    }
    qmul.io.Bl := Delay(al, delay2)
    qmul.io.Bh := Delay(ah, delay2)
    qmul.io.bb := Delay(aa, delay2)
    q := qmul.io.Q

    io.Q(7 downto 4) := p
    io.Q(3 downto 0) := q

    this.latency = qmul.latency + 2 * latencyXor + dinv.latency + latencyXor + scala.math.max(absq.latency + latencyXor, abmul.latency) + 2 * latencyXor
    true
  }
}

// Select and invert byte using MUX21I
case class SELECT_NOT_8(pipelined : Boolean = false) extends Template {
  val io = new Bundle {
    val A = in  Bits(8 bits)
    val B = in  Bits(8 bits)
    val s = in  Bool()
    val Q = out Bits(8 bits)
  }

  override def instantiate(): Boolean = {
    val mux_out = Bits(8 bits)
    when (!io.s) {
      mux_out := io.A
    } otherwise {
      mux_out := io.B
    }

    for (i <- 7 downto 0) {
      io.Q(i) := Not(mux_out(i))
    }

    this.latency = latencyNot
    true
  }
}

// A compact Sbox implementation by Canright. See:
// - D. Canright. A very compact Rijndael S-box.
// - D. Canright. A Very Compact S-Box for AES.
// This pipelined implementation is capable of generating one SboxOutput per clock cycle.
case class CanrightSbox(pipelined : Boolean = false) extends AESSbox {
  override def instantiate(): Boolean = {

    // We currently only support latencyXor = 0 for the pipelined implementations.
    // We would have to adjust some delays to make it work for arbitrary xor / not latencies.
    if (pipelined && latencyXor > 0) {
      return false
    }

    val B, C, D, X, Y, Z = Bits(8 bits)
    val R1, R2, R3, R4, R5, R6, R7, R8, R9 = Bool()
    val T1, T2, T3, T4, T5, T6, T7, T8, T9, T10 = Bool()

    val sel_in = SELECT_NOT_8(pipelined)
    val sel_out = SELECT_NOT_8(pipelined)
    val inv = GF_INV_8(pipelined)
    sel_in.instantiateTemplate()
    sel_out.instantiateTemplate()
    inv.instantiateTemplate()
    sel_in.control := control
    sel_out.control := control
    inv.control := control

    // Change basis from GF(2^8) to GF(2^8)/GF(2^4)/GF(2^2)
    // Combine with bit inverse matrix multiply of Sbox
    R1 := Xor(io.sboxIn(7), io.sboxIn(5))
    R2 := Not(Xor(io.sboxIn(7), io.sboxIn(4)))
    R3 := Xor(io.sboxIn(6), io.sboxIn(0))
    R4 := Not(Xor(io.sboxIn(5), R3))
    R5 := Xor(io.sboxIn(4), R4)
    R6 := Xor(io.sboxIn(3), io.sboxIn(0))
    R7 := Xor(io.sboxIn(2), R1)
    R8 := Xor(io.sboxIn(1), R3)
    R9 := Xor(io.sboxIn(3), R8)
    B(7) := Not(Xor(R7, R8))
    B(6) := R5
    B(5) := Xor(io.sboxIn(1), R4)
    B(4) := Not(Xor(R1, R3))
    B(3) := Xor(io.sboxIn(1), Xor(R2, R6))
    B(2) := Not(io.sboxIn(0))
    B(1) := R4
    B(0) := Not(Xor(io.sboxIn(2), R9))
    Y(7) := R2
    Y(6) := Xor(io.sboxIn(4), R8)
    Y(5) := Xor(io.sboxIn(6), io.sboxIn(4))
    Y(4) := R9
    Y(3) := Not(Xor(io.sboxIn(6), R2))
    Y(2) := R7
    Y(1) := Xor(io.sboxIn(4), R6)
    Y(0) := Xor(io.sboxIn(1), R5)

    // Calculate multiplicative inverse
    sel_in.io.A := B
    sel_in.io.B := Y
    sel_in.io.s := io.isInverseSbox
    Z := sel_in.io.Q

    inv.io.A := Z
    C := inv.io.Q

    // Change basis back from GF(2^8)/GF(2^4)/GF(2^2) to GF(2^8)
    T1 := Xor(C(7), C(3))
    T2 := Xor(C(6), C(4))
    T3 := Xor(C(6), C(0))
    T4 := Not(Xor(C(5), C(3)))
    T5 := Not(Xor(C(5), T1))
    T6 := Not(Xor(C(5), C(1)))
    T7 := Not(Xor(C(4), T6))
    T8 := Xor(C(2), T4)
    T9 := Xor(C(1), T2)
    T10 := Xor(T3, T5)
    D(7) := T4
    D(6) := T1
    D(5) := T3
    D(4) := T5
    D(3) := Xor(T2, T5)
    D(2) := Xor(T3, T8)
    D(1) := T7
    D(0) := T9
    X(7) := Not(Xor(C(4), C(1)))
    X(6) := Xor(C(1), T10)
    X(5) := Xor(C(2), T10)
    X(4) := Not(Xor(C(6), C(1)))
    X(3) := Xor(T8, T9)
    X(2) := Not(Xor(C(7), T7))
    X(1) := T6
    X(0) := Not(C(2))

    // Select the correct output
    sel_out.io.A := D
    sel_out.io.B := X
    sel_out.io.s := io.isInverseSbox
    io.sboxOut := sel_out.io.Q

    val in_change_basis_latency = scala.math.max(4 * latencyXor + 1 * latencyNot, 2 * latencyXor + 2 * latencyNot)
    val out_change_basis_latency = scala.math.max(3 * latencyXor + 3 * latencyNot, 4 * latencyXor + latencyNot)
    this.latency = sel_out.latency + out_change_basis_latency + inv.latency + sel_in.latency + in_change_basis_latency
    true
  }
}

// An even more compact Sbox implementation by Boyar and Peralta
// See: Boyar, J., Peralta, R. (2012). A Small Depth-16 Circuit for the AES S-Box. In: Gritzalis, D., Furnell, S., Theoharidou, M. (eds) Information Security and Privacy Research. SEC 2012. IFIP Advances in Information and Communication Technology, vol 376. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-642-30436-1_24
case class BoyarPeraltaSbox(pipelined : Boolean = false) extends AESSbox {
  override def instantiate(): Boolean = {

    // Define required signals
    val U = Bits(8 bits)
    val T, T_forward, T_reverse = Vec(Bool(), 27)
    val R5, R13, R17, R18, R19, Y5, D = Bool()
    val M = Vec(Bool(), 63)
    val L, P = Vec(Bool(), 30)
    val S, W = Bits(8 bits)

    // U is the sbox input in bit reversed form
    U := io.sboxIn.reversed

    // Top linear transform in forward direction
    T_forward(0) := Xor(U(0), U(3))
    T_forward(1) := Xor(U(0), U(5))
    T_forward(2) := Xor(U(0), U(6))
    T_forward(3) := Xor(U(3), U(5))
    T_forward(4) := Xor(U(4), U(6))
    T_forward(5) := Xor(T_forward(0), T_forward(4))
    T_forward(6) := Xor(U(1), U(2))
    T_forward(7) := Xor(U(7), T_forward(5))
    T_forward(8) := Xor(U(7), T_forward(6))
    T_forward(9) := Xor(T_forward(5), T_forward(6))
    T_forward(10) := Xor(U(1), U(5))
    T_forward(11) := Xor(U(2), U(5))
    T_forward(12) := Xor(T_forward(2), T_forward(3))
    T_forward(13) := Xor(T_forward(5), T_forward(10))
    T_forward(14) := Xor(T_forward(4), T_forward(10))
    T_forward(15) := Xor(T_forward(4), T_forward(11))
    T_forward(16) := Xor(T_forward(8), T_forward(15))
    T_forward(17) := Xor(U(3), U(7))
    T_forward(18) := Xor(T_forward(6), T_forward(17))
    T_forward(19) := Xor(T_forward(0), T_forward(18))
    T_forward(20) := Xor(U(6), U(7))
    T_forward(21) := Xor(T_forward(6), T_forward(20))
    T_forward(22) := Xor(T_forward(1), T_forward(21))
    T_forward(23) := Xor(T_forward(1), T_forward(9))
    T_forward(24) := Xor(T_forward(19), T_forward(16))
    T_forward(25) := Xor(T_forward(2), T_forward(15))
    T_forward(26) := Xor(T_forward(0), T_forward(11))

    // Top linear transform in reverse direction
    T_reverse(22) := Xor(U(0), U(3))
    T_reverse(21) := Not(Xor(U(1), U(3)))
    T_reverse(1) := Not(Xor(U(0), U(1)))
    T_reverse(0) := Xor(U(3), U(4))
    T_reverse(23) := Not(Xor(U(4), U(7)))
    R5 := Xor(U(6), U(7))
    T_reverse(7) := Not(Xor(U(1), T_reverse(22)))
    T_reverse(18) := Xor(T_reverse(21), R5)
    T_reverse(8) := Not(Xor(U(7), T_reverse(0)))
    T_reverse(9) := Xor(T_reverse(1), T_reverse(23))
    T_reverse(12) := Xor(T_reverse(1), R5)
    T_reverse(2) := Xor(T_reverse(0), R5)
    T_reverse(24) := Not(Xor(U(2), T_reverse(0)))
    R13 := Xor(U(1), U(6))
    T_reverse(16) := Not(Xor(U(2), T_reverse(18)))
    T_reverse(19) := Xor(T_reverse(23), R13)
    T_reverse(3) := Xor(U(4), T_reverse(7))
    R17 := Not(Xor(U(2), U(5)))
    R18 := Not(Xor(U(5), U(6)))
    R19 := Not(Xor(U(2), U(4)))
    Y5 := Xor(U(0), R17)
    T_reverse(5) := Xor(T_reverse(21), R17)
    T_reverse(15) := Xor(R13, R19)
    T_reverse(26) := Xor(T_reverse(0), R18)
    T_reverse(14) := Xor(T_reverse(9), T_reverse(26))
    T_reverse(13) := Xor(T_reverse(9), R18)
    T_reverse(25) := Xor(T_reverse(2), T_reverse(15))
    // Set remaining bits of T_reverse to 0
    T_reverse(4) := False
    T_reverse(6) := False
    T_reverse(10) := False
    T_reverse(11) := False
    T_reverse(17) := False
    T_reverse(20) := False

    // Select appropriate value for T and D depending on whether a forward or reverse Sbox is computed
    when (!io.isInverseSbox) {
      T := T_forward
      D := U(7)
    } otherwise {
      T := T_reverse
      D := Y5
    }

    val delay1 = if (pipelined) latencyAnd else 0
    val delay2 = if (pipelined) 2 * latencyAnd else 0
    val delay3 = if (pipelined) 3 * latencyAnd else 0

    // We create separate signals for delayed values which are needed multiple times.
    // This might or might not actually decrease resource utilization, but it can't hurt either.
    val M26_delayed1 = Delay(M(26), delay1)
    val M23_delayed1 = Delay(M(23), delay1)

    M(0) := And(T(12), T(5)) // 1 * AND
    M(1) := And(T(22), T(7)) // 1 * AND
    M(2) := Xor(Delay(T(13), delay1), M(0)) // 1 * AND
    M(3) := And(T(18), D) // 1 * AND
    M(4) := Xor(M(3), M(0)) // 1 * AND
    M(5) := And(T(2), T(15)) // 1 * AND
    M(6) := And(T(21), T(8)) // 1 * AND
    M(7) := Xor(Delay(T(25), delay1), M(5)) // 1 * AND
    M(8) := And(T(19), T(16)) // 1 * AND
    M(9) := Xor(M(8), M(5)) // 1 * AND
    M(10) := And(T(0), T(14)) // 1 * AND
    M(11) := And(T(3), T(26)) // 1 * AND
    M(12) := Xor(M(11), M(10)) // 1 * AND
    M(13) := And(T(1), T(9)) // 1 * AND
    M(14) := Xor(M(13), M(10)) // 1 * AND
    M(15) := Xor(M(2), M(1)) // 1 * AND
    M(16) := Xor(M(4), Delay(T(23), delay1)) // 1 * AND
    M(17) := Xor(M(7), M(6)) // 1 * AND
    M(18) := Xor(M(9), M(14)) // 1 * AND
    M(19) := Xor(M(15), M(12)) // 1 * AND
    M(20) := Xor(M(16), M(14)) // 1 * AND
    M(21) := Xor(M(17), M(12)) // 1 * AND
    M(22) := Xor(M(18), Delay(T(24), delay1)) // 1 * AND
    M(23) := Xor(M(21), M(22)) // 1 * AND
    M(24) := And(M(21), M(19)) // 2 * AND
    M(25) := Xor(Delay(M(20), delay1), M(24)) // 2 * AND
    M(26) := Xor(M(19), M(20)) // 1 * AND
    M(27) := Xor(Delay(M(22), delay1), M(24)) // 2 * AND
    M(28) := And(M(27), M26_delayed1) // 3 * AND
    M(29) := And(M(25), M23_delayed1) // 3 * AND
    M(30) := And(M(19), M(22)) // 2 * AND
    M(31) := And(M26_delayed1, M(30)) // 3 * AND
    M(32) := Xor(M26_delayed1, M(24)) // 2 * AND
    M(33) := And(M(20), M(21)) // 2 * AND
    M(34) := And(M23_delayed1, M(33)) // 3 * AND
    M(35) := Xor(M23_delayed1, M(24)) // 2 * AND
    M(36) := Xor(Delay(M(20), delay2), M(28)) // 3 * AND
    M(37) := Xor(M(31), Delay(M(32), delay1)) // 3 * AND
    M(38) := Xor(Delay(M(22), delay2), M(29)) // 3 * AND
    M(39) := Xor(M(34), Delay(M(35), delay1)) // 3 * AND
    M(40) := Xor(M(37), M(39)) // 3 * AND
    M(41) := Xor(M(36), M(38)) // 3 * AND
    M(42) := Xor(M(36), M(37)) // 3 * AND
    M(43) := Xor(M(38), M(39)) // 3 * AND
    M(44) := Xor(M(41), M(40)) // 3 * AND
    M(45) := And(M(43), Delay(T(5), delay3)) // 4 * AND
    M(46) := And(M(39), Delay(T(7), delay3)) // 4 * AND
    M(47) := And(M(38), Delay(D, delay3)) // 4 * AND
    M(48) := And(M(42), Delay(T(15), delay3)) // 4 * AND
    M(49) := And(M(37), Delay(T(8), delay3))  // 4 * AND
    M(50) := And(M(36), Delay(T(16), delay3)) // 4 * AND
    M(51) := And(M(41), Delay(T(14), delay3)) // 4 * AND
    M(52) := And(M(44), Delay(T(26), delay3)) // 4 * AND
    M(53) := And(M(40), Delay(T(9), delay3)) // 4 * AND
    M(54) := And(M(43), Delay(T(12), delay3)) // 4 * AND
    M(55) := And(M(39), Delay(T(22), delay3)) // 4 * AND
    M(56) := And(M(38), Delay(T(18), delay3)) // 4 * AND
    M(57) := And(M(42), Delay(T(2), delay3)) // 4 * AND
    M(58) := And(M(37), Delay(T(21), delay3)) // 4 * AND
    M(59) := And(M(36), Delay(T(19), delay3)) // 4 * AND
    M(60) := And(M(41), Delay(T(0), delay3)) // 4 * AND
    M(61) := And(M(44), Delay(T(3), delay3)) // 4 * AND
    M(62) := And(M(40), Delay(T(1), delay3)) // 4 * AND

    // Bottom linear transform in forward direction
    L(0) := Xor(M(60), M(61)) // 4 * AND
    L(1) := Xor(M(49), M(55)) // 4 * AND
    L(2) := Xor(M(45), M(47)) // 4 * AND
    L(3) := Xor(M(46), M(54)) // 4 * AND
    L(4) := Xor(M(53), M(57)) // 4 * AND
    L(5) := Xor(M(48), M(60)) // 4 * AND
    L(6) := Xor(M(61), L(5)) // 4 * AND
    L(7) := Xor(M(45), L(3)) // 4 * AND
    L(8) := Xor(M(50), M(58)) // 4 * AND
    L(9) := Xor(M(51), M(52)) // 4 * AND
    L(10) := Xor(M(52), L(4)) // 4 * AND
    L(11) := Xor(M(59), L(2)) // 4 * AND
    L(12) := Xor(M(47), M(50)) // 4 * AND
    L(13) := Xor(M(49), L(0)) // 4 * AND
    L(14) := Xor(M(51), M(60)) // 4 * AND
    L(15) := Xor(M(54), L(1)) // 4 * AND
    L(16) := Xor(M(55), L(0)) // 4 * AND
    L(17) := Xor(M(56), L(1)) // 4 * AND
    L(18) := Xor(M(57), L(8)) // 4 * AND
    L(19) := Xor(M(62), L(4)) // 4 * AND
    L(20) := Xor(L(0), L(1)) // 4 * AND
    L(21) := Xor(L(1), L(7)) // 4 * AND
    L(22) := Xor(L(3), L(12)) // 4 * AND
    L(23) := Xor(L(18), L(2)) // 4 * AND
    L(24) := Xor(L(15), L(9)) // 4 * AND
    L(25) := Xor(L(6), L(10)) // 4 * AND
    L(26) := Xor(L(7), L(9)) // 4 * AND
    L(27) := Xor(L(8), L(10)) // 4 * AND
    L(28) := Xor(L(11), L(14)) // 4 * AND
    L(29) := Xor(L(11), L(17)) // 4 * AND
    S(0) := Xor(L(6), L(24)) // 4 * AND
    S(1) := Not(Xor(L(16), L(26))) // 4 * AND
    S(2) := Not(Xor(L(19), L(28))) // 4* AND
    S(3) := Xor(L(6), L(21)) // 4 * AND
    S(4) := Xor(L(20), L(22)) // 4 * AND
    S(5) := Xor(L(25), L(29)) // 4 * AND
    S(6) := Not(Xor(L(13), L(27))) // 4 * AND
    S(7) := Not(Xor(L(6), L(23)))

    // Bottom linear transform in reverse direction
    P(0) := Xor(M(51), M(60)) // 4 * AND
    P(1) := Xor(M(57), M(58)) // 4 * AND
    P(2) := Xor(M(53), M(61)) // 4 * AND
    P(3) := Xor(M(46), M(49)) // 4 * AND
    P(4) := Xor(M(47), M(55)) // 4 * AND
    P(5) := Xor(M(45), M(50)) // 4 * AND
    P(6) := Xor(M(48), M(59)) // 4 * AND
    P(7) := Xor(P(0), P(1)) // 4 * AND
    P(8) := Xor(M(49), M(52)) // 4 * AND
    P(9) := Xor(M(54), M(62)) // 4 * AND
    P(10) := Xor(M(56), P(4)) // 4 * AND
    P(11) := Xor(P(0), P(3)) // 4 * AND
    P(12) := Xor(M(45), M(47)) // 4 * AND
    P(13) := Xor(M(48), M(50)) // 4 * AND
    P(14) := Xor(M(48), M(61)) // 4 * AND
    P(15) := Xor(M(53), M(58)) // 4 * AND
    P(16) := Xor(M(56), M(60)) // 4 * AND
    P(17) := Xor(M(57), P(2)) // 4 * AND
    P(18) := Xor(M(62), P(5)) // 4 * AND
    P(19) := Xor(P(2), P(3)) // 4 * AND
    P(20) := Xor(P(4), P(6)) // 4 * AND
    P(21) := False
    P(22) := Xor(P(2), P(7)) // 4 * AND
    P(23) := Xor(P(7), P(8)) // 4 * AND
    P(24) := Xor(P(5), P(7)) // 4 * AND
    P(25) := Xor(P(6), P(10)) // 4 * AND
    P(26) := Xor(P(9), P(11)) // 4 * AND
    P(27) := Xor(P(10), P(18)) // 4 * AND
    P(28) := Xor(P(11), P(25)) // 4 * AND
    P(29) := Xor(P(15), P(20)) // 4 * AND
    W(0) := Xor(P(13), P(22)) // 4 * AND
    W(1) := Xor(P(26), P(29)) // 4 * AND
    W(2) := Xor(P(17), P(28)) // 4 * AND
    W(3) := Xor(P(12), P(22)) // 4 * AND
    W(4) := Xor(P(23), P(27)) // 4 * AND
    W(5) := Xor(P(19), P(24)) // 4 * AND
    W(6) := Xor(P(14), P(23)) // 4 * AND
    W(7) := Xor(P(9), P(16)) // 4 * AND

    // Set output
    when (!io.isInverseSbox) {
      // For some reason we can't just use io.sboxOut := S.reversed
      for (i <- 7 downto 0) {
        io.sboxOut(i) := S(7 - i)
      }
    } otherwise {
      for (i <- 7 downto 0) {
        io.sboxOut(i) := W(7 - i)
      }
    }

    this.latency = 4 * latencyAnd
    true
  }
}