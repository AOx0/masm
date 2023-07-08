//! Basic assembler for x86_64
//!
//! References:
//!     - Intel® 64 and IA-32 Architectures Software Developer’s Manual
//!       Combined Volumes: 1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D, and 4
//!       At: https://cdrdv2.intel.com/v1/dl/getContent/671200

use anyhow::{anyhow, Context, Result};
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

mod bytecode;

fn generate_elf(
    out: File,
    mut sections: HashMap<&str, Fun<'_>>,
    mut data: Vec<Data<'_>>,
) -> Result<()> {
    let name = "out.o";

    let target = triple!("x86_64-unknown-unknown-elf");
    let mut obj = ArtifactBuilder::new(target).name(name.to_owned()).finish();

    let (_, start) = sections
        .remove_entry("_start")
        .context("main or _start must be defined")?;

    obj.declarations(
        sections
            .iter()
            .map(|(name, _)| (name, Decl::function().into())),
    )?;
    obj.declarations(
        [("_start", Decl::function().global().into())]
            .iter()
            .cloned(),
    )?;
    obj.declarations(data.iter().map(|dat| (dat.name, Decl::data().into())))?;

    sections.iter_mut().try_for_each(|(name, sect)| {
        obj.define(name, std::mem::replace(&mut sect.bytecode, Vec::new()))
    })?;
    obj.define("_start", start.bytecode)?;
    data.iter_mut()
        .try_for_each(|dat| obj.define(dat.name, std::mem::replace(&mut dat.values, Vec::new())))?;
    start
        .references
        .into_iter()
        .try_for_each(|link| obj.link(link.into()))?;

    sections.into_values().try_for_each(|v| {
        v.references
            .into_iter()
            .try_for_each(|link| obj.link(link.into()))
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
pub struct Ref<'a> {
    from: &'a str,
    to: &'a str,
    at: u64,
}

#[derive(Debug)]
enum DataSize {
    B1 = 0x1,
    B2 = 0x2,
    B4 = 0x4,
    B8 = 0x8,
    // ba = 0xA,
}

#[derive(Debug)]
pub struct Data<'a> {
    name: &'a str,
    size: DataSize,
    values: Vec<u8>,
}

impl<'a> Data<'a> {
    fn from_str(input: &'a str) -> Result<Data<'a>> {
        let (name, decl) = input
            .split_once(':')
            .map(|(name, decl)| (name.trim(), decl.trim()))
            .context(anyhow!("Could not get name declaration NAME: DECL"))?;

        let purp = match &decl[0..2] {
            "db" => DataSize::B1,
            "dw" => DataSize::B2,
            "dd" => DataSize::B4,
            "dq" => DataSize::B8,
            // "dt" => DataSize::ba,
            _ => return Err(anyhow!("Purpose {} is not valid", &decl[0..2])),
        };

        let mut values: Vec<u8> = vec![];

        decl[2..]
            .split(",")
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
                        c.chars().next().context(anyhow!(""))? as usize
                    } else {
                        return Err(anyhow!(
                            "Values starting with ' should be only chars. Ex: 'a'"
                        ));
                    }
                } else {
                    bytecode::parse_number(&val)
                        .context(anyhow!("Failed to get number from {}", val))?
                };

                // TODO: Verificar que el dato si cabe en el objetivo
                match purp {
                    DataSize::B1 => values.extend(&(number as u8).to_le_bytes()),
                    DataSize::B2 => values.extend(&(number as u16).to_le_bytes()),
                    DataSize::B4 => values.extend(&(number as u32).to_le_bytes()),
                    DataSize::B8 => values.extend(&(number as u64).to_le_bytes()),
                };

                Ok::<_, anyhow::Error>(())
            })?;

        Ok(Data {
            name,
            size: purp,
            values,
        })
    }
}

impl<'a> Into<faerie::Link<'a>> for Ref<'a> {
    fn into(self) -> faerie::Link<'a> {
        Link {
            from: self.from,
            to: self.to,
            at: self.at,
        }
    }
}

#[derive(Debug)]
pub struct Fun<'a> {
    name: &'a str,
    instructions: Vec<&'a str>,
    references: Vec<Ref<'a>>,
    bytecode: Vec<u8>,
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
        bytecode: vec![],
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

    let mut data = vec![];
    let mut sections = get_segments(&script);
    for ref mut section in sections.iter_mut() {
        if section.name == "main" {
            section.name = "_start"
        }

        if section.name == "data" {
            for ins in section.instructions.iter() {
                let d = Data::from_str(ins).unwrap();
                println!("{:X?}", d);
                data.push(d);
            }
        }

        for ins in section.instructions.iter() {
            if ins.starts_with("mov ") {
                section
                    .bytecode
                    .extend_from_slice(&bytecode::mov::Move::from_str(ins).unwrap().into_bytes())
            } else if ins == &"syscall" {
                section.bytecode.extend_from_slice(&[0xF, 0x5])
            } else if ins.starts_with("call") {
                let a = bytecode::call::Call::from_str(ins, &section.bytecode).unwrap();

                section.references.push(a.get_ref(section.name));
                section.bytecode.extend_from_slice(&a.into_bytes());
            }
        }

        println!("Name: {:?}", section.name);
        println!("    {:?}", section.instructions);
        println!("    {:?}", section.references);
        println!("    {:X?}", section.bytecode);
    }

    let sections: HashMap<&str, Fun<'_>> =
        HashMap::from_iter(sections.into_iter().map(|a| (a.name, a)));

    generate_elf(out, sections, data)
}
