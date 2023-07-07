use anyhow::{anyhow, Result};
use std::str::FromStr;

pub mod call;
pub mod mov;

#[derive(Debug, Clone, Copy)]
enum Register {
    AX = 0b000,
    CX = 0b001,
    DX = 0b010,
    BX = 0b011,
    SP = 0b100,
    BP = 0b101,
    SI = 0b110,
    DI = 0b111,
}

#[derive(Debug, Clone, Copy)]
enum RegisterBits {
    Bit64(Register),
    Bit32(Register),
    Bit16(Register),
}

impl RegisterBits {
    fn set_reg(&mut self, reg: Register) {
        match self {
            RegisterBits::Bit64(ref mut a) => {
                *a = reg;
            }
            RegisterBits::Bit32(ref mut a) => {
                *a = reg;
            }
            RegisterBits::Bit16(ref mut a) => {
                *a = reg;
            }
        }
    }

    fn parse_reg(input: &str) -> Option<RegisterBits> {
        let mut reg_bits = match input.chars().next()? {
            'r' => RegisterBits::Bit64(Register::AX),
            'e' => RegisterBits::Bit32(Register::AX),
            'a' | 'c' | 'd' | 'b' | 's' => RegisterBits::Bit16(Register::AX),
            _ => return None,
        };

        let kword = if matches!(reg_bits, RegisterBits::Bit64(_) | RegisterBits::Bit32(_)) {
            1
        } else {
            0
        };

        reg_bits.set_reg(match &input[kword..] {
            "ax" => Register::AX,
            "cx" => Register::CX,
            "dx" => Register::DX,
            "bx" => Register::BX,
            "sp" => Register::SP,
            "bp" => Register::BP,
            "si" => Register::SI,
            "di" => Register::DI,
            _ => unreachable!("Not implemented register {}", &input),
        });

        Some(reg_bits)
    }
}

#[derive(Debug, Clone, Copy)]
enum Either<U, V> {
    Left(U),
    Right(V),
}

#[derive(Debug, Clone, Copy)]
enum Imm {
    Bit8(u8),
    Bit16(u16),
    Bit32(u32),
    Bit64(u64),
}

impl Imm {
    fn infer_from_reg_and_val(reg: RegisterBits, val: usize) -> Imm {
        match reg {
            RegisterBits::Bit64(_) => {
                if val <= u32::MAX as usize {
                    Imm::Bit32(0)
                } else {
                    Imm::Bit64(0)
                }
            }
            RegisterBits::Bit32(_) => Imm::Bit32(0),
            RegisterBits::Bit16(_) => Imm::Bit16(0),
        }
    }

    fn set_val(&mut self, val: usize) -> Result<()> {
        match self {
            Imm::Bit64(ref mut pval) => {
                if val <= u64::MAX as usize {
                    *pval = val as u64
                } else {
                    return Err(anyhow!("Value {} bigger than 64 bit", val));
                }
            }
            Imm::Bit32(ref mut pval) => {
                if val <= u32::MAX as usize {
                    *pval = val as u32
                } else {
                    return Err(anyhow!("Value {} bigger than 64 bit", val));
                }
            }
            Imm::Bit16(ref mut pval) => {
                if val <= u16::MAX as usize {
                    *pval = val as u16
                } else {
                    return Err(anyhow!("Value {} bigger than 64 bit", val));
                }
            }
            Imm::Bit8(ref mut pval) => {
                if val <= u8::MAX as usize {
                    *pval = val as u8
                } else {
                    return Err(anyhow!("Value {} bigger than 64 bit", val));
                }
            }
        }

        Ok(())
    }
}

pub fn parse_number(input: &str) -> Option<usize> {
    // Attempt to parse hexadecimal representation
    if input.starts_with("0x") {
        usize::from_str_radix(&input[2..], 16).ok()
    }
    // Attempt to parse binary representation
    else if input.starts_with("0b") {
        usize::from_str_radix(&input[2..], 2).ok()
    }
    // Parse decimal representation
    else {
        usize::from_str(input).ok()
    }
}

#[derive(Debug)]
enum Rex {
    Rex = 0b0100_0000,
    RexW = 0b0100_1000,
}
