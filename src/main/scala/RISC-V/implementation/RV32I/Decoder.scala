package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.model._
import RISCV.interfaces.RV32I.AbstractDecoder

class Decoder extends AbstractDecoder {
  val opcode = RISCV_OP(io_decoder.instr(6, 0))
  val funct3 = RISCV_FUNCT3(io_decoder.instr(14, 12))
  val funct7 = RISCV_FUNCT7(io_decoder.instr(31, 25))
  val funct12 = RISCV_FUNCT12(io_decoder.instr(31, 20))

  val RS1 = io_decoder.instr(19, 15)
  val RS2 = io_decoder.instr(24, 20)
  val RD = io_decoder.instr(11, 7)
  //
  // define default values
  io_decoder.rs1 := 0.U
  io_decoder.rs2 := 0.U
  io_decoder.rd := 0.U
  io_decoder.imm := 0.U

  switch(opcode) {
    is(RISCV_OP.OP) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := RD
      io_decoder.imm := Fill(32, 0.U)
    }
    is(RISCV_OP.OP_IMM) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        20
      )
    }
    is(RISCV_OP.LUI) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.AUIPC) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.BRANCH) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(19, io_decoder.instr(31)) ## io_decoder.instr(
        31
      ) ## io_decoder.instr(7) ## io_decoder.instr(30, 25) ## io_decoder.instr(
        11,
        8
      ) ## Fill(1, 0.U)
    }
    is(RISCV_OP.STORE) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        25
      ) ## io_decoder.instr(11, 7)
    }


    //Task1
    //  jal rd, imm     跳转到 pc + imm的地方      然后回来的地方是rd 让rd = pc + 4 即可
    //31        30..21       20        19..12       11..7    6..0

    //imm[20]   imm[10:1]    imm[11]   imm[19:12]   rd       opcode 
    // imm[0] = 0 末位要补0所以是 21位， 但 imm需要32位digit 前面sign extend 符号位 11次
    is(RISCV_OP.JAL) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD


      io_decoder.imm := Fill(11, io_decoder.instr(31)) ##
                        io_decoder.instr(31) ##
                        io_decoder.instr(19, 12) ##
                        io_decoder.instr(20) ##
                        io_decoder.instr(30, 21) ## 
                        0.U(1.W)


    }
    // jalr rd, rs1, imm
    // 31..20        19..15   14..12   11..7   6..0
    // imm[11:0]     rs1      funct3   rd      opcode           无须补0
    is(RISCV_OP.JALR) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ##
                        io_decoder.instr(31, 20) 
      }
  }
  
}
