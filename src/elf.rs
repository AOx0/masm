use super::*;

pub fn generate(
    out: File,
    functions: HashSet<Fun>,
    defines: HashSet<DataDefine>,
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
            .map(|dat| (dat.name.clone(), Decl::data().with_writable(true).into())),
    )?;
    obj.declarations(
        externs
            .iter()
            .map(|name| (name.clone(), Decl::function_import().into())),
    )?;

    sections
        .iter_mut()
        .try_for_each(|(name, sect)| obj.define(name, take(&mut sect.bytecode)))?;
    obj.define("_start", start.bytecode)?;
    defines
        .iter()
        .try_for_each(|dat| obj.define(dat.name.clone(), dat.data.clone()))?;

    for link in start
        .references
        .iter()
        .chain(sections.values().flat_map(|s| s.references.iter()))
    {
        if !externs.contains(&link.to)
            && !defines.iter().any(|a| a.name == link.to)
            && !sections.contains_key(&link.to)
            && start.name != link.to
        {
            bail!("Undefined reference to {} at {}", link.to, link.from);
        }
    }

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
