use anyhow::{anyhow, Result};
use clap::Parser;
use faerie::*;
use nom::IResult;
use std::collections::HashMap;
use std::str::FromStr;
use std::{
    fs::{File, OpenOptions},
    path::PathBuf,
};
use target_lexicon::triple;

fn parse_number(input: &str) -> Option<usize> {
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

fn generate_elf(out: File, sections: &HashMap<&str, Fun<'_>>) -> Result<()> {
    let name = "test.o";

    let target = triple!("x86_64-unknown-unknown-elf");
    let mut obj = ArtifactBuilder::new(target).name(name.to_owned()).finish();

    obj.declarations(
        [("_start", Decl::function().global().into())]
            .iter()
            .cloned(),
    )?;

    obj.define("_start", sections["_start"].bytecode.clone())?;

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

#[derive(Debug)]
enum Mnemonic {}

fn read_script(path: &std::path::Path) -> Result<String> {
    use std::io::Read;

    let mut script = String::with_capacity(5000);

    let mut file = OpenOptions::new()
        .read(true)
        .write(false)
        .create(false)
        .open(path)?;

    file.read_to_string(&mut script)?;

    // Remove comments
    let mut script = script
        .lines()
        .filter_map(|a| {
            if a.trim_start().starts_with(";") || a.trim_start().is_empty() {
                None
            } else {
                a.split_once(";")
                    .and_then(|(a, _)| Some(a))
                    .unwrap_or_else(|| a)
                    .trim_end()
                    .into()
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    script.push_str("\n");

    Ok(script)
}

#[derive(Debug)]
struct Fun<'a> {
    name: &'a str,
    instructions: Vec<&'a str>,
    references: Vec<Ref<'a>>,
    bytecode: Vec<u8>,
}

#[derive(Debug)]
struct Ref<'a> {
    symbol: &'a str,
    position: usize,
    from: &'a str,
    typ: RefType,
}

#[derive(Debug)]
enum RefType {
    Call,
}

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

#[derive(Debug)]
struct Move {
    into: RegisterBits,
    value: Either<RegisterBits, Imm>,
}

enum Rex {
    Rex = 0b0100_0000,
    RexW = 0b0100_1000,
}

impl Move {
    fn from_str(input: &str) -> Option<Move> {
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

    fn into_bytes(&self) -> Vec<u8> {
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

use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{multispace0, space0},
    combinator::map,
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated},
};

fn parse_section(input: &str) -> IResult<&str, Fun> {
    let extras = terminated(take_until("\n"), tag("\n"));
    let parse_name = terminated(take_until(":"), preceded(tag(":"), extras));
    let parse_member = terminated(preceded(tag("    "), take_until("\n")), tag("\n"));

    let parse_members = many0(parse_member);

    let parse_segment = map(pair(parse_name, parse_members), |(name, members)| Fun {
        name,
        instructions: members,
        references: vec![],
        bytecode: vec![],
    });

    delimited(space0, parse_segment, multispace0)(input)
}

fn parse_sections(input: &str) -> IResult<&str, Vec<Fun>> {
    many1(parse_section)(input)
}

fn get_segments(input: &str) -> Vec<Fun> {
    let (_, sections) = parse_sections(input.trim_start()).unwrap();
    sections
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

    println!("{script:?}");

    let mut sections = get_segments(&script);
    for ref mut section in sections.iter_mut() {
        if section.name == "main" {
            section.name = "_start"
        }
        for (i, ins) in section.instructions.iter().enumerate() {
            if ins.contains('<') {
                let reff = if ins.contains("call") {
                    Some(Ref {
                        symbol: ins,
                        position: i,
                        from: section.name,
                        typ: RefType::Call,
                    })
                } else {
                    None
                };

                reff.map(|a| section.references.push(a));
            }

            if ins.starts_with("mov") {
                section
                    .bytecode
                    .extend_from_slice(&Move::from_str(ins).unwrap().into_bytes())
            } else if ins.contains("syscall") {
                section.bytecode.extend_from_slice(&[0xF, 0x5])
            }
        }

        println!("Name: {}", section.name);
        println!("    {:?}", section.instructions);
        println!("    {:?}", section.references);
        println!("    {:X?}", section.bytecode);
    }

    let sections: HashMap<&str, Fun<'_>> =
        HashMap::from_iter(sections.into_iter().map(|a| (a.name, a)));

    generate_elf(out, &sections)
}
