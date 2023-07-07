use anyhow::Result;
use clap::Parser;
use faerie::*;
use nom::IResult;
use std::str::FromStr;
use std::{
    fs::{File, OpenOptions},
    path::PathBuf,
};
use target_lexicon::triple;

fn generate_elf(out: File) -> Result<()> {
    let name = "test.o";

    let target = triple!("x86_64-unknown-unknown-elf");
    let mut obj = ArtifactBuilder::new(target).name(name.to_owned()).finish();

    obj.declarations(
        [
            ("end", Decl::function().into()),
            ("_start", Decl::function().global().into()),
            ("msg", Decl::cstring().into()),
        ]
        .iter()
        .cloned(),
    )?;

    // 0000000000000000 <end>:
    //    0:	48 c7 c0 3c 00 00 00 	mov    $0x3c,%rax
    //    7:	48 c7 c7 00 00 00 00 	mov    $0x0,%rdi
    //    e:	0f 05                	syscall
    obj.define(
        "end",
        vec![
            0x48, 0xc7, 0xc0, 0x3c, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00,
            0x0f, 0x05,
        ],
    )?;

    // 0000000000000000 <_start>:
    //    0:	48 c7 c0 01 00 00 00 	mov    $0x1,%rax
    //    7:	48 c7 c7 01 00 00 00 	mov    $0x1,%rdi
    //    e:	48 8d 35 00 00 00 00 	lea    0x0(%rip),%rsi        # 15 <_start+0x15>
    //   15:	48 ba 0d 00 00 00 00 	movabs $0xd,%rdx
    //   1c:	00 00 00
    //   1f:	0f 05                	syscall
    //   21:	e8 00 00 00 00       	call   26 <_start+0x26>
    obj.define(
        "_start",
        vec![
            0x48, 0xc7, 0xc0, 0x01, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc7, 0x01, 0x00, 0x00, 0x00,
            0x48, 0x8d, 0x35, 0x00, 0x00, 0x00, 0x00, 0x48, 0xba, 0x0d, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x0f, 0x05, 0xe8, 0x00, 0x00, 0x00, 0x00,
        ],
    )?;

    obj.define("msg", b"Hola Mundo!\n\0".to_vec())?;

    // Next, we declare our relocations,
    // which are _always_ relative to the `from` symbol
    obj.link(Link {
        from: "_start",
        to: "msg",
        at: 17,
    })?;
    obj.link(Link {
        from: "_start",
        to: "end",
        at: 34,
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

struct Move {
    into: RegisterBits,
    value: Either<RegisterBits, Imm>,
}

enum Rex {
    Rex = 0b0100_0000,
    RexW = 0b0100_1000,
}

impl Move {
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

    let sections = get_segments(&script);
    for mut section in sections {
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
        }

        println!("Name: {}", section.name);
        println!("    {:?}", section.instructions);
        println!("    {:?}", section.references);
    }

    let test = Move {
        into: RegisterBits::Bit16(Register::CX),
        value: Either::Left(RegisterBits::Bit16(Register::CX)),
    };

    println!("{:X?}", test.into_bytes());

    generate_elf(out)
}
