#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]

//! Basic assembler for x86_64
//!
//! References:
//!     - Intel® 64 and IA-32 Architectures Software Developer’s Manual
//!       Combined Volumes: 1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D, and 4
//!       At: https://cdrdv2.intel.com/v1/dl/getContent/671200
#![allow(dead_code)]

use std::collections::HashMap;

use std::str::FromStr;
use std::{
    fs::{File, OpenOptions},
    path::PathBuf,
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use faerie::*;
use num::Bounded;

use target_lexicon::Triple;

mod parser;

#[derive(Debug, Clone, Copy)]
enum Bits {
    Bits8 = 8,
    Bits16 = 16,
    Bits32 = 32,
    Bits64 = 64,
}

impl PartialEq<u8> for Bits {
    fn eq(&self, other: &u8) -> bool {
        *self as u8 == *other
    }
}

impl From<Bits> for u8 {
    fn from(bits: Bits) -> Self {
        bits as u8
    }
}

#[derive(Debug, Clone, Copy)]
struct Register {
    reg: RegisterName,
    bits: Bits,
}

impl From<Register> for u8 {
    fn from(register: Register) -> Self {
        register.reg as u8
    }
}

impl FromStr for Register {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let inp = inp.trim();
        let (bits, reg) = if inp.starts_with('r') {
            (Bits::Bits64, inp.trim_start_matches('r'))
        } else if inp.starts_with('e') {
            (Bits::Bits32, inp.trim_start_matches('e'))
        } else if inp.len() == 2 {
            (Bits::Bits16, inp)
        } else {
            return Err(anyhow!("Invalid register: {}", inp));
        };

        let reg = match reg {
            "ax" => RegisterName::AX,
            "cx" => RegisterName::CX,
            "dx" => RegisterName::DX,
            "bx" => RegisterName::BX,
            "sp" => RegisterName::SP,
            "bp" => RegisterName::BP,
            "si" => RegisterName::SI,
            "di" => RegisterName::DI,
            _ => return Err(anyhow!("Invalid register: {}", inp)),
        };

        Ok(Register { reg, bits })
    }
}

#[derive(Debug, Clone, Copy)]
enum RegisterName {
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
enum Immediate {
    SByte(i8),
    SWord(i16),
    SDword(i32),
    SQword(i64),
    Byte(u8),
    Word(u16),
    Dword(u32),
    Qword(u64),
}

impl From<Immediate> for Bits {
    fn from(immediate: Immediate) -> Self {
        match immediate {
            Immediate::Byte(_) | Immediate::SByte(_) => Bits::Bits8,
            Immediate::Word(_) | Immediate::SWord(_) => Bits::Bits16,
            Immediate::Dword(_) | Immediate::SDword(_) => Bits::Bits32,
            Immediate::Qword(_) | Immediate::SQword(_) => Bits::Bits64,
        }
    }
}

impl From<Immediate> for Vec<u8> {
    fn from(immediate: Immediate) -> Self {
        match immediate {
            Immediate::Byte(a) => vec![a],
            Immediate::Word(a) => a.to_le_bytes().to_vec(),
            Immediate::Dword(a) => a.to_le_bytes().to_vec(),
            Immediate::Qword(a) => a.to_le_bytes().to_vec(),
            Immediate::SByte(a) => vec![a as u8],
            Immediate::SWord(a) => a.to_le_bytes().to_vec(),
            Immediate::SDword(a) => a.to_le_bytes().to_vec(),
            Immediate::SQword(a) => a.to_le_bytes().to_vec(),
        }
    }
}

impl Immediate {
    fn into_signed(self, bits: Bits) -> Result<Self> {
        match self {
            Immediate::SByte(val) => match bits {
                Bits::Bits8 => Ok(Immediate::SByte(val)),
                Bits::Bits16 => Ok(Immediate::SWord(val as i16)),
                Bits::Bits32 => Ok(Immediate::SDword(val as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::SWord(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as i16 {
                        Err(anyhow!("Cannot convert word to byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => Ok(Immediate::SWord(val)),
                Bits::Bits32 => Ok(Immediate::SDword(val as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::SDword(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as i32 {
                        Err(anyhow!("Cannot convert dword to byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => {
                    if val > i16::max_value() as i32 {
                        Err(anyhow!("Cannot convert dword to word"))
                    } else {
                        Ok(Immediate::SWord(val as i16))
                    }
                }
                Bits::Bits32 => Ok(Immediate::SDword(val)),
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::SQword(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as i64 {
                        Err(anyhow!("Cannot convert qword to byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => {
                    if val > i16::max_value() as i64 {
                        Err(anyhow!("Cannot convert qword to word"))
                    } else {
                        Ok(Immediate::SWord(val as i16))
                    }
                }
                Bits::Bits32 => {
                    if val > i32::max_value() as i64 {
                        Err(anyhow!("Cannot convert qword to dword"))
                    } else {
                        Ok(Immediate::SDword(val as i32))
                    }
                }
                Bits::Bits64 => Ok(Immediate::SQword(val)),
            },
            Immediate::Byte(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as u8 {
                        Err(anyhow!("Cannot convert byte to signed byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => Ok(Immediate::SWord(val as i16)),
                Bits::Bits32 => Ok(Immediate::SDword(val as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::Word(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as u16 {
                        Err(anyhow!("Cannot convert word to signed byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => {
                    if val > i16::max_value() as u16 {
                        Err(anyhow!("Cannot convert word to signed word"))
                    } else {
                        Ok(Immediate::SWord(val as i16))
                    }
                }
                Bits::Bits32 => Ok(Immediate::SDword(val as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::Dword(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as u32 {
                        Err(anyhow!("Cannot convert dword to signed byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => {
                    if val > i16::max_value() as u32 {
                        Err(anyhow!("Cannot convert dword to signed word"))
                    } else {
                        Ok(Immediate::SWord(val as i16))
                    }
                }
                Bits::Bits32 => {
                    if val > i32::max_value() as u32 {
                        Err(anyhow!("Cannot convert dword to signed dword"))
                    } else {
                        Ok(Immediate::SDword(val as i32))
                    }
                }
                Bits::Bits64 => Ok(Immediate::SQword(val as i64)),
            },
            Immediate::Qword(val) => match bits {
                Bits::Bits8 => {
                    if val > i8::max_value() as u64 {
                        Err(anyhow!("Cannot convert qword to signed byte"))
                    } else {
                        Ok(Immediate::SByte(val as i8))
                    }
                }
                Bits::Bits16 => {
                    if val > i16::max_value() as u64 {
                        Err(anyhow!("Cannot convert qword to signed word"))
                    } else {
                        Ok(Immediate::SWord(val as i16))
                    }
                }
                Bits::Bits32 => {
                    if val > i32::max_value() as u64 {
                        Err(anyhow!("Cannot convert qword to signed dword"))
                    } else {
                        Ok(Immediate::SDword(val as i32))
                    }
                }
                Bits::Bits64 => {
                    if val > i64::max_value() as u64 {
                        Err(anyhow!("Cannot convert qword to signed qword"))
                    } else {
                        Ok(Immediate::SQword(val as i64))
                    }
                }
            },
        }
    }

    fn into_imm(self, bits: Bits) -> Result<Self> {
        match self {
            Immediate::Byte(a) => match bits {
                Bits::Bits8 => Ok(Immediate::Byte(a)),
                Bits::Bits16 => Ok(Immediate::Word(a as u16)),
                Bits::Bits32 => Ok(Immediate::Dword(a as u32)),
                Bits::Bits64 => Ok(Immediate::Qword(a as u64)),
            },
            Immediate::Word(a) => match bits {
                Bits::Bits8 => {
                    if a > u8::MAX as u16 {
                        Err(anyhow!("Cannot convert word to byte"))
                    } else {
                        Ok(Immediate::Byte(a as u8))
                    }
                }
                Bits::Bits16 => Ok(Immediate::Word(a)),
                Bits::Bits32 => Ok(Immediate::Dword(a as u32)),
                Bits::Bits64 => Ok(Immediate::Qword(a as u64)),
            },
            Immediate::Dword(a) => match bits {
                Bits::Bits8 => {
                    if a > u8::MAX as u32 {
                        Err(anyhow!("Cannot convert dword to byte"))
                    } else {
                        Ok(Immediate::Byte(a as u8))
                    }
                }
                Bits::Bits16 => {
                    if a > u16::MAX as u32 {
                        Err(anyhow!("Cannot convert dword to word"))
                    } else {
                        Ok(Immediate::Word(a as u16))
                    }
                }
                Bits::Bits32 => Ok(Immediate::Dword(a)),
                Bits::Bits64 => Ok(Immediate::Qword(a as u64)),
            },
            Immediate::Qword(a) => match bits {
                Bits::Bits8 => {
                    if a > u8::MAX as u64 {
                        Err(anyhow!("Cannot convert qword to byte"))
                    } else {
                        Ok(Immediate::Byte(a as u8))
                    }
                }
                Bits::Bits16 => {
                    if a > u16::MAX as u64 {
                        Err(anyhow!("Cannot convert qword to word"))
                    } else {
                        Ok(Immediate::Word(a as u16))
                    }
                }
                Bits::Bits32 => {
                    if a > u32::MAX as u64 {
                        Err(anyhow!("Cannot convert qword to dword"))
                    } else {
                        Ok(Immediate::Dword(a as u32))
                    }
                }
                Bits::Bits64 => Ok(Immediate::Qword(a)),
            },
            Immediate::SByte(a) => match bits {
                Bits::Bits8 => Ok(Immediate::SByte(a)),
                Bits::Bits16 => Ok(Immediate::SWord(a as i16)),
                Bits::Bits32 => Ok(Immediate::SDword(a as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(a as i64)),
            },
            Immediate::SWord(a) => match bits {
                Bits::Bits8 => {
                    if a > i8::max_value() as i16 {
                        Err(anyhow!("Cannot convert sword to byte"))
                    } else {
                        Ok(Immediate::SByte(a as i8))
                    }
                }
                Bits::Bits16 => Ok(Immediate::SWord(a)),
                Bits::Bits32 => Ok(Immediate::SDword(a as i32)),
                Bits::Bits64 => Ok(Immediate::SQword(a as i64)),
            },
            Immediate::SDword(a) => match bits {
                Bits::Bits8 => {
                    if a > i8::max_value() as i32 {
                        Err(anyhow!("Cannot convert sdword to byte"))
                    } else {
                        Ok(Immediate::SByte(a as i8))
                    }
                }
                Bits::Bits16 => {
                    if a > i16::max_value() as i32 {
                        Err(anyhow!("Cannot convert sdword to word"))
                    } else {
                        Ok(Immediate::SWord(a as i16))
                    }
                }
                Bits::Bits32 => Ok(Immediate::SDword(a)),
                Bits::Bits64 => Ok(Immediate::SQword(a as i64)),
            },
            Immediate::SQword(a) => match bits {
                Bits::Bits8 => {
                    if a > i8::max_value() as i64 {
                        Err(anyhow!("Cannot convert sqword to byte"))
                    } else {
                        Ok(Immediate::SByte(a as i8))
                    }
                }
                Bits::Bits16 => {
                    if a > i16::max_value() as i64 {
                        Err(anyhow!("Cannot convert sqword to word"))
                    } else {
                        Ok(Immediate::SWord(a as i16))
                    }
                }
                Bits::Bits32 => {
                    if a > i32::max_value() as i64 {
                        Err(anyhow!("Cannot convert sqword to dword"))
                    } else {
                        Ok(Immediate::SDword(a as i32))
                    }
                }
                Bits::Bits64 => Ok(Immediate::SQword(a)),
            },
        }
    }

    fn parse(inp: &str) -> Result<Immediate> {
        if inp.starts_with("0x") {
            Self::parse_hex_0x(inp)
        } else if inp.ends_with('h') || inp.ends_with('H') {
            Self::parse_hex_h(inp)
        } else if inp.starts_with("0b") {
            Self::parse_bin_0b(inp)
        } else if inp.ends_with('b') || inp.ends_with('B') {
            Self::parse_bin_b(inp)
        } else if !inp.starts_with('-') {
            Self::parse_decimal(inp)
        } else {
            Self::parse_signed_decimal(inp)
        }
    }

    fn parse_hex_0x(inp: &str) -> Result<Immediate> {
        let inp = inp.trim_start_matches("0x").trim_start_matches("0X");
        Self::parse_hex(inp)
    }

    fn parse_hex_h(inp: &str) -> Result<Immediate> {
        let inp = inp.trim_end_matches('h').trim_end_matches('H');
        Self::parse_hex(inp)
    }

    fn parse_hex(inp: &str) -> Result<Immediate> {
        if let Ok(a) = u8::from_str_radix(inp, 16) {
            Ok(Immediate::Byte(a))
        } else if let Ok(a) = u16::from_str_radix(inp, 16) {
            Ok(Immediate::Word(a))
        } else if let Ok(a) = u32::from_str_radix(inp, 16) {
            Ok(Immediate::Dword(a))
        } else if let Ok(a) = u64::from_str_radix(inp, 16) {
            Ok(Immediate::Qword(a))
        } else {
            Err(anyhow!("Invalid hex literal: {}", inp))
        }
    }

    fn parse_bin_0b(inp: &str) -> Result<Immediate> {
        let inp = inp.trim_start_matches("0b").trim_start_matches("0B");
        Self::parse_bin(inp)
    }

    fn parse_bin_b(inp: &str) -> Result<Immediate> {
        let inp = inp.trim_end_matches('B').trim_end_matches('b');
        Self::parse_bin(inp)
    }

    fn parse_bin(inp: &str) -> Result<Immediate> {
        if let Ok(a) = u8::from_str_radix(inp, 2) {
            Ok(Immediate::Byte(a))
        } else if let Ok(a) = u16::from_str_radix(inp, 2) {
            Ok(Immediate::Word(a))
        } else if let Ok(a) = u32::from_str_radix(inp, 2) {
            Ok(Immediate::Dword(a))
        } else if let Ok(a) = u64::from_str_radix(inp, 2) {
            Ok(Immediate::Qword(a))
        } else {
            Err(anyhow!("Invalid binary literal: {}", inp))
        }
    }

    fn parse_decimal(inp: &str) -> Result<Immediate> {
        if let Ok(a) = inp.parse::<u8>() {
            Ok(Immediate::Byte(a))
        } else if let Ok(a) = inp.parse::<u16>() {
            Ok(Immediate::Word(a))
        } else if let Ok(a) = inp.parse::<u32>() {
            Ok(Immediate::Dword(a))
        } else if let Ok(a) = inp.parse::<u64>() {
            Ok(Immediate::Qword(a))
        } else {
            Err(anyhow!("Invalid decimal literal: {}", inp))
        }
    }

    fn parse_signed_decimal(inp: &str) -> Result<Immediate> {
        if let Ok(a) = inp.parse::<i8>() {
            Ok(Immediate::SByte(a))
        } else if let Ok(a) = inp.parse::<i16>() {
            Ok(Immediate::SWord(a))
        } else if let Ok(a) = inp.parse::<i32>() {
            Ok(Immediate::SDword(a))
        } else if let Ok(a) = inp.parse::<i64>() {
            Ok(Immediate::SQword(a))
        } else {
            Err(anyhow!("Invalid decimal literal: {}", inp))
        }
    }
}

impl FromStr for Immediate {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, <Self as FromStr>::Err> {
        Immediate::parse(inp.trim())
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
}

impl FromStr for Symbol {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let (_, remain) = parse_name()(inp)?;

        if !remain.is_empty() {
            Err(anyhow!("Invalid symbol: {}", inp))
        } else {
            Ok(Symbol {
                name: inp.to_string(),
            })
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
enum Scale {
    #[default]
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
}

impl From<Scale> for u8 {
    fn from(scale: Scale) -> Self {
        match scale {
            Scale::One => 0b00,
            Scale::Two => 0b01,
            Scale::Four => 0b10,
            Scale::Eight => 0b11,
        }
    }
}

impl FromStr for Scale {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        if let Immediate::Byte(val) = Immediate::parse(inp.trim())? {
            match val {
                1 => Ok(Scale::One),
                2 => Ok(Scale::Two),
                4 => Ok(Scale::Four),
                8 => Ok(Scale::Eight),
                _ => Err(anyhow!("Invalid scale: {}", inp)),
            }
        } else {
            Err(anyhow!("Invalid scale: {}", inp))
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Displacement {
    Byte(i8),
    Word(i16),
    Dword(i32),
}

impl From<Displacement> for Immediate {
    fn from(displacement: Displacement) -> Self {
        match displacement {
            Displacement::Byte(val) => Immediate::SByte(val),
            Displacement::Word(val) => Immediate::SWord(val),
            Displacement::Dword(val) => Immediate::SDword(val),
        }
    }
}

impl From<Displacement> for Bits {
    fn from(displacement: Displacement) -> Self {
        match displacement {
            Displacement::Byte(_) => Bits::Bits8,
            Displacement::Word(_) => Bits::Bits16,
            Displacement::Dword(_) => Bits::Bits32,
        }
    }
}

impl From<Displacement> for Vec<u8> {
    fn from(displacement: Displacement) -> Self {
        match displacement {
            Displacement::Byte(val) => vec![val as u8],
            Displacement::Word(val) => val.to_le_bytes().to_vec(),
            Displacement::Dword(val) => val.to_le_bytes().to_vec(),
        }
    }
}

impl Default for Displacement {
    fn default() -> Self {
        Displacement::Dword(0)
    }
}

impl FromStr for Displacement {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let number = Immediate::parse(inp.trim())?.into_signed(Bits::Bits32)?;
        let number = if let Ok(imm) = number.into_signed(Bits::Bits8) {
            imm
        } else {
            number
        };

        match number {
            Immediate::SByte(val) => Ok(Displacement::Byte(val)),
            Immediate::SWord(val) => Ok(Displacement::Word(val)),
            Immediate::SDword(val) => Ok(Displacement::Dword(val)),
            _ => Err(anyhow!("Invalid displacement: {}", inp)),
        }
    }
}

/// En caso de que se usen valores por default, se debe usar el valor de 101 como base (copia de rsp) y el valor de 100 como index (rsp)
/// con escala de 0b00, efectivamente haciendo que no haya indexado adicional
#[derive(Debug, Clone, Copy)]
struct EffectiveAddress {
    base: Register,
    index: Register,
    scale: Scale,
    displacement: Displacement,
    typ: EffectiveAddressType,
}

impl EffectiveAddress {
    fn into_vec(self, reg: Register) -> Result<Vec<u8>> {
        let mem = self;
        let only_base = matches!(mem.typ, EffectiveAddressType::OnlyBase);
        let mut extender = vec![];
        if only_base {
            let mut modrm = 0b00_000_000;
            modrm += u8::from(mem.base);
            modrm += u8::from(reg) << 3;
            extender.push(modrm);
        } else {
            let mut modrm = 0b00_000_100;
            modrm += u8::from(reg) << 3;

            let mut sib = 0b00_000_000;
            sib += u8::from(mem.base);
            sib += u8::from(mem.index) << 3;
            sib += u8::from(mem.scale) << 6;

            let mut imm = Immediate::from(mem.displacement).into_imm(Bits::Bits32)?;

            if !matches!(mem.typ, EffectiveAddressType::OnlyDisplacement) {
                imm = if let Ok(imm) = imm.into_imm(Bits::Bits8) {
                    imm
                } else {
                    imm.into_imm(Bits::Bits32)?
                };

                modrm += if Bits::from(imm) as u8 == 8 {
                    0b01_000_000
                } else {
                    0b10_000_000
                };
            }

            extender.push(modrm);
            extender.push(sib);
            extender.extend_from_slice(&Vec::from(imm));
        }

        Ok(extender)
    }
}

impl Default for EffectiveAddress {
    fn default() -> Self {
        EffectiveAddress {
            base: Register {
                bits: Bits::Bits64,
                reg: RegisterName::BP,
            },
            index: Register {
                bits: Bits::Bits64,
                reg: RegisterName::SP,
            },
            scale: Scale::One,
            displacement: Displacement::default(),
            typ: EffectiveAddressType::Else,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum EffectiveAddressType {
    OnlyDisplacement,
    OnlyBase,
    Else,
}

/// Cases:
///
///     [base]
///     [base + displacement]
///     [displacement]
///     [base + index]
///     [base + index + displacement]
///     [base + index * scale]
///     [base + index * scale + displacement]
impl FromStr for EffectiveAddress {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let mut result = EffectiveAddress::default();
        let original = inp;
        let inp = inp.trim_start_matches('[').trim_end_matches(']');
        let inp = inp.replace(" ", "");
        let mut parts = inp.split('+');
        let part = parts.next().unwrap().trim();

        // Case [displacement]
        if Immediate::parse(part).is_ok() {
            result.displacement = Displacement::from_str(part)?;
            result.typ = EffectiveAddressType::OnlyDisplacement;
            return Ok(result);
        } else if let Ok(base) = Register::from_str(part) {
            // Else get register
            result.base = base;

            if parts.clone().collect::<Vec<_>>().join("").trim().is_empty() {
                result.typ = EffectiveAddressType::OnlyBase;
                return Ok(result);
            }
        } else {
            return Err(anyhow!("Invalid effective address: {}", original));
        }

        let part = parts.next().map(|p| p.trim());
        if let Some(part) = part {
            // Case [base + displacement]
            if let Ok(displacement) = Displacement::from_str(part.trim()) {
                result.typ = EffectiveAddressType::Else;
                result.displacement = displacement;
                return Ok(result);
            }
        }

        // Else try get [base + index * scale] where scale may be missing  [base + index]
        if let Some(mut parts) = part {
            let (index, scale) = parts.split_once('*').unwrap_or((parts, ""));

            let index = Register::from_str(index.trim())?;
            result.typ = EffectiveAddressType::Else;
            result.index = index;
            if !scale.is_empty() {
                result.scale = Scale::from_str(scale)?;
            }
        } else {
            return Ok(result);
        }

        // Try get [base + index * scale + displacement]
        if let Some(part) = parts.next() {
            if let Ok(displacement) = Displacement::from_str(part.trim()) {
                result.typ = EffectiveAddressType::Else;
                result.displacement = displacement;
                return Ok(result);
            }
        } else {
            return Ok(result);
        }

        Err(anyhow!("Invalid effective address: {}", original))
    }
}

#[derive(Debug, Clone)]
enum Operand {
    Register(Register),
    Immediate(Immediate),
    Memory(EffectiveAddress),
    Symbol(Symbol),
}

impl FromStr for Operand {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let inp = inp.trim();
        // TODO: Make this crash on error instead of cascading
        if let Ok(register) = Register::from_str(inp) {
            Ok(Operand::Register(register))
        } else if let Ok(immediate) = Immediate::from_str(inp) {
            Ok(Operand::Immediate(immediate))
        } else if let Ok(effective_address) = EffectiveAddress::from_str(inp) {
            Ok(Operand::Memory(effective_address))
        } else if let Ok(symbol) = Symbol::from_str(inp) {
            Ok(Operand::Symbol(symbol))
        } else {
            Err(anyhow!("Invalid operand: {}", inp))
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy)]
enum Mn {
    MOV,
    CALL,
    RET,
    PUSH,
    POP,
    ADD,
    SUB,
    MUL,
    DIV,
    JMP,
    JE,
    JNE,
    JG,
    JGE,
    JL,
    JLE,
    CMP,
    LEA,
    NOP,
    INT,
    SYSCALL,
    LEAVE,
    CLD,
    STD,
    STI,
    CLI,
    HLT,
}

impl FromStr for Mn {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        match inp.to_ascii_uppercase().as_str() {
            "MOV" => Ok(Mn::MOV),
            "CALL" => Ok(Mn::CALL),
            "RET" => Ok(Mn::RET),
            "PUSH" => Ok(Mn::PUSH),
            "POP" => Ok(Mn::POP),
            "ADD" => Ok(Mn::ADD),
            "SUB" => Ok(Mn::SUB),
            "MUL" => Ok(Mn::MUL),
            "DIV" => Ok(Mn::DIV),
            "JMP" => Ok(Mn::JMP),
            "JE" => Ok(Mn::JE),
            "JNE" => Ok(Mn::JNE),
            "JG" => Ok(Mn::JG),
            "JGE" => Ok(Mn::JGE),
            "JL" => Ok(Mn::JL),
            "JLE" => Ok(Mn::JLE),
            "CMP" => Ok(Mn::CMP),
            "LEA" => Ok(Mn::LEA),
            "NOP" => Ok(Mn::NOP),
            "INT" => Ok(Mn::INT),
            "SYSCALL" => Ok(Mn::SYSCALL),
            "LEAVE" => Ok(Mn::LEAVE),
            "CLD" => Ok(Mn::CLD),
            "STD" => Ok(Mn::STD),
            "STI" => Ok(Mn::STI),
            "CLI" => Ok(Mn::CLI),
            "HLT" => Ok(Mn::HLT),
            _ => Err(anyhow!("Invalid mnemonic: {}", inp)),
        }
    }
}

#[derive(Debug, Clone)]
struct Mnemonic {
    name: Mn,
    operands: Vec<Operand>,
    line: usize,
}

impl FromStr for Mnemonic {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let mut parts = inp.splitn(2, ' ');
        let name = parts.next().context(anyhow!("Invalid mnemonic: {}", inp))?;
        let operands = parts
            .next()
            .map(|p| p.split(',').map(|p| p.trim()).collect::<Vec<_>>())
            .unwrap_or_default();

        let name = Mn::from_str(name)?;
        let operands = operands
            .into_iter()
            .map(Operand::from_str)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Mnemonic {
            name,
            operands,
            line: 0,
        })
    }
}

#[derive(Debug, Clone)]
struct DataDefine {
    name: String,
    data: Vec<u8>,
    purpose: Bits,
}

impl FromStr for DataDefine {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> Result<Self, Self::Err> {
        let (name, decl) = inp
            .split_once(' ')
            .map(|(name, decl)| (name.trim(), decl.trim()))
            .context(anyhow!("Could not get name declaration NAME: DECL"))?;

        let purp = match &decl[0..2] {
            "db" => Bits::Bits8,
            "dw" => Bits::Bits16,
            "dd" => Bits::Bits32,
            "dq" => Bits::Bits64,
            // "dt" => DataSize::ba,
            _ => return Err(anyhow!("Purpose {} is not valid", &decl[0..2])),
        };

        let mut values: Vec<u8> = vec![];

        decl[2..]
            .split(',')
            .map(|val| val.trim())
            .filter_map(|val| {
                if val.is_empty() {
                    None
                } else if val.starts_with('\"') {
                    Some(
                        val.trim_matches('\"')
                            .chars()
                            .map(|c| format!("'{}'", c))
                            .collect::<Vec<_>>(),
                    )
                } else {
                    Some(vec![val.to_owned()])
                }
            })
            .flatten()
            .try_for_each(|val| {
                let number = if val.starts_with('\'') {
                    let c = val.trim_matches('\'');

                    if c.len() == 1 {
                        Immediate::Byte(c.chars().next().context(anyhow!(""))? as u8)
                    } else {
                        return Err(anyhow!(
                            "Values starting with ' should be only chars. Ex: 'a'"
                        ));
                    }
                } else {
                    Immediate::parse(&val).context(anyhow!("Failed to get number from {}", val))?
                };

                match number.into_imm(purp).or_else(|_| number.into_signed(purp)) {
                    Ok(imm) => values.extend_from_slice(&Vec::from(number)),
                    Err(_) => return Err(anyhow!("Invalid number {} for purpose {:?}", val, purp)),
                }

                Ok::<_, anyhow::Error>(())
            })?;

        Ok(DataDefine {
            name: name.to_string(),
            purpose: purp,
            data: values,
        })
    }
}

#[derive(Debug, Clone)]
enum Line {
    Mnemonic(Mnemonic),
    DataDefine(DataDefine),
    Extern(String),
    Comment(String),
    Function(Fun),
    Empty,
}

fn parse_name() -> impl parser::Parser<String> {
    use parser::*;
    let parse_name = match_until_err(match_any_of(
        [
            CharMatch::Char('_'),
            CharMatch::CharRange('a'..='z'),
            CharMatch::CharRange('0'..='9'),
        ]
        .to_vec(),
    ));
    let parse_space = match_until_err(match_any_of(
        [CharMatch::Char(' '), CharMatch::Char('\t')].to_vec(),
    ));

    map(and_then(parse_name, parse_space), |(c, r)| {
        (format!("{:?}", c), r)
    })
}

impl FromStr for Line {
    type Err = anyhow::Error;
    fn from_str(inp: &str) -> std::result::Result<Self, Self::Err> {
        let original = inp;
        let inp = inp.trim();

        if inp.trim_start().starts_with(';') {
            return Ok(Line::Comment(original.to_string()));
        } else if inp.trim().is_empty() {
            return Ok(Line::Empty);
        }

        let inp = inp
            .split_once(';')
            .map(|(a, _)| a)
            .unwrap_or_else(|| inp)
            .trim_end();

        use parser::*;

        let is_definition: for<'a> fn(&'a str) -> Result<bool> = |inp| {
            let inp = inp.trim_start().to_ascii_lowercase();

            let (_, rem) = parse_name()(&inp)?;
            Ok(["db", "dw", "dd", "dq", "dt"]
                .iter()
                .any(|s| rem.starts_with(s)))
        };

        let is_function: for<'a> fn(&'a str) -> Result<bool> = |inp: &str| {
            let inp = inp.trim_start();
            let (_, rem) = parse_name()(inp)?;
            Ok(rem.trim().starts_with(':'))
        };

        let is_definition = is_definition(inp).unwrap_or(false);

        let lower = inp.to_ascii_lowercase();
        let res = if !is_definition
            && [" db ", " dw ", " dd ", " dq ", " dt "]
                .iter()
                .any(|s| lower.contains(s))
        {
            return Err(anyhow!("Invalid data definition {}", inp));
        } else if is_definition {
            Line::DataDefine(DataDefine::from_str(inp)?)
        } else if lower.trim_start().contains("extern") {
            Line::Extern(inp.trim_start_matches("extern").trim().to_string())
        } else if is_function(inp).unwrap_or(false) {
            Line::Function(Fun {
                name: inp.trim().trim_end_matches(':').to_string(),
                lines: vec![],
                references: vec![],
                bytecode: vec![],
            })
        } else {
            Line::Mnemonic(Mnemonic::from_str(inp)?)
        };

        Ok(res)
    }
}

fn generate_elf(
    out: File,
    functions: Vec<Fun>,
    mut defines: Vec<DataDefine>,
    externs: Vec<String>,
) -> Result<()> {
    let name = "out.o";

    let target = Triple {
        architecture: target_lexicon::Architecture::X86_64,
        vendor: target_lexicon::Vendor::Unknown,
        operating_system: target_lexicon::OperatingSystem::Unknown,
        environment: target_lexicon::Environment::Unknown,
        binary_format: if cfg!(target_os = "macos") {
            target_lexicon::BinaryFormat::Macho
        } else {
            target_lexicon::BinaryFormat::Elf
        },
    };
    let mut obj = ArtifactBuilder::new(target).name(name.to_owned()).finish();

    let mut sections: HashMap<String, Fun> =
        HashMap::from_iter(functions.into_iter().map(|a| (a.name.clone(), a)));

    let (_, start) = sections
        .remove_entry("_start")
        .context("main or _start must be defined")?;

    obj.declarations(sections.keys().map(|name| (name, Decl::function().into())))?;
    obj.declarations(
        [("_start", Decl::function().global().into())]
            .iter()
            .cloned(),
    )?;
    obj.declarations(
        defines
            .iter()
            .map(|dat| (dat.name.clone(), Decl::data().into())),
    )?;
    obj.declarations(
        externs
            .iter()
            .map(|name| (name.clone(), Decl::function_import().into())),
    )?;

    sections
        .iter_mut()
        .try_for_each(|(name, sect)| obj.define(name, std::mem::take(&mut sect.bytecode)))?;
    obj.define("_start", start.bytecode)?;
    defines
        .iter_mut()
        .try_for_each(|dat| obj.define(dat.name.clone(), std::mem::take(&mut dat.data)))?;
    start
        .references
        .into_iter()
        .try_for_each(|link| obj.link(link.to_link()))?;

    sections.into_values().try_for_each(|v| {
        v.references
            .into_iter()
            .try_for_each(|link| obj.link(link.to_link()))
    })?;

    // Finally, we write the object file
    obj.write(out)?;
    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File path to compile
    #[arg(long, short)]
    path: PathBuf,
    /// Path to store the compiled file
    #[arg(long, short, default_value = "./out.o")]
    output: PathBuf,
}

fn read_script(path: &std::path::Path) -> Result<String> {
    use std::io::Read;

    let mut script = String::with_capacity(5000);

    let mut file = OpenOptions::new()
        .read(true)
        .write(false)
        .create(false)
        .open(path)?;

    file.read_to_string(&mut script)?;
    script.push('\n');
    Ok(script)
}

#[derive(Debug, Clone)]
pub struct Ref {
    from: String,
    to: String,
    at: u64,
}

impl Ref {
    fn to_link(&self) -> Link {
        Link {
            from: &self.from,
            to: &self.to,
            at: self.at,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    name: String,
    lines: Vec<Line>,
    references: Vec<Ref>,
    bytecode: Vec<u8>,
}

impl Fun {
    fn assemble(&mut self) -> Result<&mut Vec<u8>> {
        self.bytecode.clear();

        static REX_V: u8 = 0x40;
        static REX_X: u8 = 0x42;
        static REX_B: u8 = 0x44;
        static REX_W: u8 = 0x48;

        for line in self.lines.iter_mut() {
            if let Line::Mnemonic(mnemonic) = line {
                let Mnemonic {
                    name,
                    ref mut operands,
                    line: i,
                } = mnemonic;
                match name {
                    Mn::MOV => {
                        if operands.len() != 2 {
                            return Err(anyhow!("Error at line {}: MOV must have 2 operands", i));
                        }

                        let mut op2 = operands.pop().unwrap();
                        let mut op1 = operands.pop().unwrap();

                        let reverse_direction = matches!(op2, Operand::Register(_))
                            && matches!(op1, Operand::Memory(_));
                        if reverse_direction {
                            std::mem::swap(&mut op1, &mut op2)
                        }

                        match (op1, op2) {
                            (Operand::Register(reg1), Operand::Register(reg2)) => {
                                let mut extender = vec![];

                                if reg1.bits as u8 == 64 && reg2.bits as u8 == 64 {
                                    extender.push(REX_W);
                                } else if reg1.bits as u8 == 16 && reg2.bits as u8 == 16 {
                                    extender.push(0x66);
                                }

                                extender.extend_from_slice(&[
                                    0x89,
                                    0b1100_0000 + reg1.reg as u8 + ((reg2.reg as u8) << 3),
                                ]);
                                self.bytecode.extend_from_slice(&extender);
                            }
                            (Operand::Register(mut reg), Operand::Immediate(imm)) => {
                                let mut opcode = vec![];

                                if let (true, Ok(imm)) =
                                    (reg.bits as u8 == 64, imm.into_imm(Bits::Bits32))
                                {
                                    reg.bits = Bits::Bits32;
                                    opcode.extend_from_slice(&[REX_W, 0xC7, 0xC0 + reg.reg as u8]);
                                } else {
                                    if reg.bits as u8 == 64 {
                                        opcode.push(REX_W);
                                    } else if reg.bits as u8 == 16 {
                                        opcode.push(0x66);
                                    }

                                    opcode.extend_from_slice(&[0xB8 + reg.reg as u8]);
                                }

                                self.bytecode.extend_from_slice(&opcode);
                                self.bytecode
                                    .extend_from_slice(&Vec::from(imm.into_imm(reg.bits)?));
                            }
                            (Operand::Register(reg), Operand::Memory(mem)) => {
                                let mut extender = vec![];
                                let mut opcode = 0x88;

                                if !matches!(reg.bits, Bits::Bits8) {
                                    opcode |= 0b0000_0001;
                                }

                                if matches!(reg.bits, Bits::Bits64) {
                                    extender.push(REX_W);
                                } else if matches!(reg.bits, Bits::Bits16) {
                                    extender.push(0x66);
                                }

                                if !reverse_direction {
                                    opcode |= 0b0000_0010;
                                }
                                extender.push(opcode);

                                self.bytecode.extend_from_slice(&extender);
                                self.bytecode.extend_from_slice(&mem.into_vec(reg)?);
                            }
                            (Operand::Register(reg), Operand::Symbol(sym)) => {
                                if reg.bits as u8 == 64 {
                                    self.bytecode.push(REX_W);
                                } else if reg.bits as u8 == 16 {
                                    self.bytecode.push(0x66);
                                }
                                self.bytecode.extend_from_slice(&[0xB8 + reg.reg as u8]);

                                self.references.push(Ref {
                                    from: self.name.clone(),
                                    to: sym.name,
                                    at: self.bytecode.len() as u64,
                                });
                                self.bytecode.extend_from_slice(&Vec::from(
                                    Immediate::Byte(0).into_imm(reg.bits)?,
                                ));
                            }
                            _ => {}
                        }
                    }
                    Mn::CALL => {
                        self.bytecode.push(0xe8);
                        if let Operand::Symbol(sym) = &operands[0] {
                            self.references.push(Ref {
                                from: self.name.clone(),
                                to: sym.name.clone(),
                                at: self.bytecode.len() as u64,
                            });
                            self.bytecode.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
                        } else {
                            return Err(anyhow!(
                                "Error at line {}: Invalid operand for CALL, must be a symbol",
                                i
                            ));
                        }
                    }
                    Mn::RET => {
                        self.bytecode.push(0xc3);
                    }
                    Mn::PUSH => match &operands[0] {
                        Operand::Register(reg) => {
                            if reg.bits as u8 == 64 {
                                self.bytecode.push(REX_W);
                            } else if reg.bits as u8 == 16 {
                                self.bytecode.push(0x66);
                            } else if reg.bits as u8 == 32 {
                                return Err(anyhow!(
                                    "Error at line {}: 32 bit mode not supported",
                                    i
                                ));
                            }

                            self.bytecode.push(0x50 + reg.reg as u8);
                        }
                        Operand::Immediate(num) => {
                            self.bytecode.push(0x68);
                            self.bytecode
                                .extend_from_slice(&Vec::from(num.into_imm(Bits::Bits32)?));
                        }
                        _ => {
                            return Err(anyhow!("Error at line {}: Invalid operand for PUSH", i));
                        }
                    },
                    Mn::POP => {}
                    Mn::ADD => {}
                    Mn::SUB => {}
                    Mn::MUL => {}
                    Mn::DIV => {}
                    Mn::JMP => {}
                    Mn::JE => {}
                    Mn::JNE => {}
                    Mn::JG => {}
                    Mn::JGE => {}
                    Mn::JL => {}
                    Mn::JLE => {}
                    Mn::CMP => {}
                    Mn::LEA => {}
                    Mn::NOP => {}
                    Mn::INT => {}
                    Mn::SYSCALL => self.bytecode.extend_from_slice(&[0x0F, 0x05]),
                    Mn::LEAVE => {}
                    Mn::CLD => {}
                    Mn::STD => {}
                    Mn::STI => {}
                    Mn::CLI => {}
                    Mn::HLT => {}
                }
            }
        }

        Ok(&mut self.bytecode)
    }
}

fn get_segments(input: &str) -> Result<Vec<Line>> {
    let mut result = Vec::new();

    for (i, line) in input.lines().enumerate() {
        match Line::from_str(line.trim()) {
            Ok(parsed) => match parsed {
                Line::DataDefine(_) => {
                    result.push(parsed);
                }
                Line::Extern(_) => {
                    result.push(parsed);
                }
                Line::Function(_) => {
                    result.push(parsed);
                }
                Line::Mnemonic(mut mnemonic) => {
                    if let Some(last) = result.last_mut() {
                        if let Line::Function(fun) = last {
                            mnemonic.line = i + 1;
                            fun.lines.push(Line::Mnemonic(mnemonic));
                        } else {
                            return Err(anyhow!(
                                "Mnemonic outside of function at line {}:\n\n    {}\n",
                                i,
                                line
                            ));
                        }
                    } else {
                        return Err(anyhow!(
                            "Mnemonic outside of function at line {}:\n\n    {}\n",
                            i,
                            line
                        ));
                    }
                }
                Line::Comment(_) | Line::Empty => {}
            },
            Err(e) => {
                return Err(anyhow!("Error at line {}: {:?}\n\n    {}\n", i, e, line));
            }
        }
    }

    Ok(result)
}

fn main() -> Result<()> {
    let Args { path, output } = Args::parse();

    let script = read_script(&path)?;

    let out = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .append(false)
        .read(false)
        .open(output)?;

    // let mut data = vec![];
    let sections = match get_segments(&script) {
        Ok(sections) => sections,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    let mut data_define = vec![];
    let mut exts = vec![];
    let mut functions = vec![];
    for section in sections.into_iter() {
        match section {
            Line::DataDefine(data) => {
                data_define.push(data);
            }
            Line::Extern(ext) => {
                exts.push(ext);
            }
            Line::Function(mut fun) => {
                if fun.name == "main" {
                    fun.name = "_start".to_string();
                }
                match fun.assemble() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}", e);
                        std::process::exit(1);
                    }
                }

                functions.push(fun.clone());
            }
            _ => {}
        }
    }

    generate_elf(out, functions, data_define, exts)
}
