use super::*;

pub fn generate(
    out: File,
    mut sections: HashMap<String, Fun>,
    mut defines: HashMap<String, DataDefine>,
    externs: HashSet<String>,
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

    let start = sections.remove_entry("_start");
    let start = start.iter().map(|(_, data)| data).next();

    obj.declarations(sections.values().map(|fun| {
        if fun.global {
            (fun.name.clone(), Decl::function().global().into())
        } else {
            (fun.name.clone(), Decl::function().into())
        }
    }))?;

    if start.is_some() {
        obj.declarations(
            [("_start", Decl::function().global().into())]
                .iter()
                .cloned(),
        )?;
    }

    obj.declarations(defines.iter().map(|(name, dat)| {
        if dat.import {
            (dat.name.clone(), Decl::data_import().into())
        } else {
            (dat.name.clone(), Decl::data().with_writable(true).into())
        }
    }))?;
    obj.declarations(
        externs
            .iter()
            .map(|name| (name.clone(), Decl::function_import().into())),
    )?;

    sections
        .iter_mut()
        .try_for_each(|(name, sect)| obj.define(name, take(&mut sect.bytecode)))?;
    if let Some(ref start) = start {
        obj.define("_start", start.bytecode.clone())?;
    }
    defines
        .iter_mut()
        .try_for_each(|(name, dat)| obj.define(name.clone(), dat.data.clone()))?;

    if let Some(ref start) = start {
        for link in start
            .references
            .iter()
            .chain(sections.values().flat_map(|s| s.references.iter()))
        {
            if !externs.contains(&link.to)
                && !defines.iter().any(|a| a.0.as_str() == link.to)
                && !sections.contains_key(&link.to)
                && start.name != link.to
            {
                bail!("Undefined reference to {} at {}", link.to, link.from);
            }
        }

        start
            .references
            .iter()
            .try_for_each(|link| obj.link(link.to_link()))?;
    }

    sections.into_values().try_for_each(|v| {
        v.references
            .into_iter()
            .try_for_each(|link| obj.link(link.to_link()))
    })?;

    // Finally, we write the object file
    obj.write(out)?;
    Ok(())
}
