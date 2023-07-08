use super::*;
use crate::DataSize;
use call::Call;

#[derive(Debug)]
pub struct Sym<'a> {
    sym: &'a str,
    start: usize,
    typ: DataSize,
}

#[derive(Debug)]
pub struct Move<'a> {
    into: RegisterBits,
    value: Either3<RegisterBits, Sym<'a>, Imm>,
}

impl<'a> Move<'a> {
    pub fn from_str(
        input: &'a str,
        current_bytecode: &[u8],
        data: &Vec<Data<'a>>,
    ) -> Option<Move<'a>> {
        if input.starts_with("mov ") {
            let parts = input[4..].split(',').map(|f| f.trim()).collect::<Vec<_>>();

            let first = parts.get(0)?;
            let second = parts.get(1)?;
            let dest = RegisterBits::parse_reg(&first)?;

            // Try parse a reg
            if let Some(from) = RegisterBits::parse_reg(&second) {
                return Some(Move {
                    into: dest,
                    value: Either3::Left(from),
                });
            } else
            // Try parse numeric value
            if let Some(val) = parse_number(&second) {
                let mut lmm = Imm::infer_from_reg_and_val(dest, val);

                lmm.set_val(val).unwrap();

                return Some(Move {
                    into: dest,
                    value: Either3::Right(lmm),
                });
            } else {
                if let Some(sym) = data.iter().find(|&dat| &dat.name == second) {
                    let mut lmm = Imm::infer_from_reg(dest);

                    lmm.set_val(0x0).unwrap();

                    return Some(Move {
                        into: dest,
                        value: Either3::Right(lmm),
                    });
                } else {
                    None
                }
            }
        } else {
            None
        }
    }

    pub fn into_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(5);

        match (self.into, self.value) {
            // MOV reg, reg
            (RegisterBits::Bit64(into), Either3::Left(RegisterBits::Bit64(from))) => result
                .extend_from_slice(&[
                    Rex::RexW as u8,
                    0x89,
                    0b1100_0000 + into as u8 + ((from as u8) << 3),
                ]),
            (RegisterBits::Bit32(into), Either3::Left(RegisterBits::Bit32(from))) => {
                result.extend_from_slice(&[0x89, 0b1100_0000 + into as u8 + ((from as u8) << 3)])
            }
            (RegisterBits::Bit16(into), Either3::Left(RegisterBits::Bit16(from))) => result
                .extend_from_slice(&[0x66, 0x89, 0b1100_0000 + into as u8 + ((from as u8) << 3)]),

            // MOV reg, val
            (RegisterBits::Bit64(reg), Either3::Right(Imm::Bit64(val))) => {
                result.extend_from_slice(&[Rex::RexW as u8, 0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit32(reg), Either3::Right(Imm::Bit32(val))) => {
                result.extend_from_slice(&[0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit16(reg), Either3::Right(Imm::Bit16(val))) => {
                result.extend_from_slice(&[0x66, 0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit64(reg), Either3::Right(Imm::Bit32(val))) => {
                // See: Intel® 64 and IA-32 Architectures Software Developer’s Manual. Vol. 2A 2-5
                // MOD: 11 (reg), OPCODE: /0
                result.extend_from_slice(&[Rex::RexW as u8, 0xC7, 0xC0 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            _ => {
                unimplemented!("MOV variant not yet implemented")
            }
        }

        result
    }
}
