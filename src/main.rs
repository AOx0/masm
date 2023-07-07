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

fn generate_elf(out: File, mut sections: HashMap<&str, Fun<'_>>) -> Result<()> {
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
    obj.declarations([("_start", Decl::function().global().into())].iter().cloned())?;

    sections.iter_mut().try_for_each(|(name, sect)| {
        obj.define(name, std::mem::replace(&mut sect.bytecode, vec![]))
    })?;
    obj.define("_start", start.bytecode)?;

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

    let mut sections = get_segments(&script);
    for ref mut section in sections.iter_mut() {
        if section.name == "main" {
            section.name = "_start"
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

    generate_elf(out, sections)
}
