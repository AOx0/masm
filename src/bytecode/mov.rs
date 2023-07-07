use super::*;

#[derive(Debug)]
pub struct Move {
    into: RegisterBits,
    value: Either<RegisterBits, Imm>,
}

impl Move {
    pub fn from_str(input: &str) -> Option<Move> {
        if input.starts_with("mov ") {
            let parts = input[4..].split(',').map(|f| f.trim()).collect::<Vec<_>>();

            let first = parts.get(0)?;
            let second = parts.get(1)?;
            let dest = RegisterBits::parse_reg(&first)?;

            if let Some(from) = RegisterBits::parse_reg(&second) {
                Some(Move {
                    into: dest,
                    value: Either::Left(from),
                })
            } else {
                let val = parse_number(&second)?;
                let mut lmm = Imm::infer_from_reg_and_val(dest, val);

                lmm.set_val(val).unwrap();

                Some(Move {
                    into: dest,
                    value: Either::Right(lmm),
                })
            }
        } else {
            None
        }
    }

    pub fn into_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(5);

        match (self.into, self.value) {
            // MOV reg, reg
            (RegisterBits::Bit64(into), Either::Left(RegisterBits::Bit64(from))) => result
                .extend_from_slice(&[
                    Rex::RexW as u8,
                    0x89,
                    0b1100_0000 + into as u8 + ((from as u8) << 3),
                ]),
            (RegisterBits::Bit32(into), Either::Left(RegisterBits::Bit32(from))) => {
                result.extend_from_slice(&[0x89, 0b1100_0000 + into as u8 + ((from as u8) << 3)])
            }
            (RegisterBits::Bit16(into), Either::Left(RegisterBits::Bit16(from))) => result
                .extend_from_slice(&[0x66, 0x89, 0b1100_0000 + into as u8 + ((from as u8) << 3)]),

            // MOV reg, val
            (RegisterBits::Bit64(reg), Either::Right(Imm::Bit64(val))) => {
                result.extend_from_slice(&[Rex::RexW as u8, 0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit32(reg), Either::Right(Imm::Bit32(val))) => {
                result.extend_from_slice(&[0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit16(reg), Either::Right(Imm::Bit16(val))) => {
                result.extend_from_slice(&[0x66, 0xB8 + reg as u8]);
                result.extend_from_slice(&val.to_le_bytes());
            }
            (RegisterBits::Bit64(reg), Either::Right(Imm::Bit32(val))) => {
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
