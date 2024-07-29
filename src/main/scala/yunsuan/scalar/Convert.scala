package yunsuan.scalar

import chisel3._
import chisel3.util._
object I2fType {
  def width = 5
  // todo:  move/cvt ## i64/i32(input) ## f64/f32/f16(output) ## hassign
  def fcvt_h_wu       = "b00000".U(width.W)
  def fcvt_h_w        = "b00001".U(width.W)
  def fcvt_h_lu       = "b01000".U(width.W)
  def fcvt_h_l        = "b01001".U(width.W)

  def fcvt_s_wu       = "b00010".U(width.W)
  def fcvt_s_w        = "b00011".U(width.W)
  def fcvt_s_lu       = "b01010".U(width.W)
  def fcvt_s_l        = "b01011".U(width.W)

  def fcvt_d_wu       = "b00100".U(width.W)
  def fcvt_d_w        = "b00101".U(width.W)
  def fcvt_d_lu       = "b01100".U(width.W)
  def fcvt_d_l        = "b01101".U(width.W)

  //  def fmv_w_x         = "b1000".U(4.W)
  //  def fmv_d_x         = "b1110".U(4.W)
}

class I2fCvtIO extends Bundle{
  val src = Input(UInt(64.W))
  val opType = Input(UInt(5.W))
  val rm = Input(UInt(3.W))
  val wflags = Input(Bool())
  val rmInst = Input(UInt(3.W))

  val result = Output(UInt(64.W))
  val fflags = Output(UInt(5.W))
}
class INT2FP(latency: Int, XLEN: Int) extends Module {
  val io = IO(new I2fCvtIO)
  val rm = Mux(io.rmInst === "b111".U, io.rm, io.rmInst)
  val regEnables = IO(Input(Vec(latency, Bool())))
  dontTouch(regEnables)
  // stage1
  val in = io.src
  val wflags = io.wflags
  val typeIn = io.opType(3)
  val typeOut = io.opType(2,1)
  val signIn = io.opType(0)
  val intValue = RegEnable(Mux(wflags,
    Mux(typeIn,
      Mux(!signIn, ZeroExt(in, XLEN), SignExt(in, XLEN)),
      Mux(!signIn, ZeroExt(in(31, 0), XLEN), SignExt(in(31, 0), XLEN))
    ),
    in
  ), regEnables(0))
  val typeInReg = RegEnable(typeIn, regEnables(0))
  val typeOutReg = RegEnable(typeOut, regEnables(0))
  val signInReg = RegEnable(signIn, regEnables(0))
  val wflagsReg = RegEnable(wflags, regEnables(0))
  val rmReg = RegEnable(rm, regEnables(0))

  // stage2
  val s2_typeInReg = typeInReg
  val s2_signInReg = signInReg
  val s2_typeOutReg = typeOutReg
  val s2_wflags = wflagsReg
  val s2_rmReg = rmReg

  val mux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })

  mux.data := intValue
  mux.exc := 0.U

  when(s2_wflags){
    val i2fResults = for(t <- FPU.ftypes) yield {
      val i2f = Module(new IntToFP(t.expWidth, t.precision))
      i2f.io.sign := s2_signInReg
      i2f.io.long := s2_typeInReg
      i2f.io.int := intValue
      i2f.io.rm := s2_rmReg
      (i2f.io.result, i2f.io.fflags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(s2_typeOutReg)
    mux.exc := VecInit(exc)(s2_typeOutReg)
  }

  // stage 3
  val s3_out = RegEnable(mux, regEnables(1))
  val s3_tag = RegEnable(s2_typeOutReg, regEnables(1))

  io.fflags := s3_out.exc
  io.result := FPU.box(s3_out.data, s3_tag)
}
