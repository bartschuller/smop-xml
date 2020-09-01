mod runner;
mod smop;
mod sxd;
use smop::SmopRunner;
use sxd::SXDRunner;
mod driver;
mod model;
mod reporter;

use crate::model::{EnvironmentSpec, TestSet};
use clap::{crate_description, crate_name, crate_version, App, Arg};
use driver::Driver;
use roxmltree::Node;
use std::collections::HashMap;
use std::path::Path;
use sxd_document::Package;

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .about(crate_description!())
        .arg(
            Arg::with_name("catalog")
                .help("path to the catalog.xml")
                .required(true),
        )
        .get_matches();
    let catalog = matches.value_of("catalog").unwrap();
    println!("Going to read catalog from {}", catalog);
    let text = std::fs::read_to_string(catalog).unwrap();
    let doc = roxmltree::Document::parse(&text).unwrap();
    let root_catalog = doc.root_element();
    let envs_and_sets: Vec<Node> = root_catalog.children().filter(|n| n.is_element()).collect();
    let (envs, test_sets): (Vec<_>, Vec<_>) = envs_and_sets
        .into_iter()
        .partition(|n| n.has_tag_name("environment"));
    let environments: HashMap<String, EnvironmentSpec> = envs
        .iter()
        .map(|node| {
            let env = EnvironmentSpec::new(node, Path::new(catalog));
            (env.name.as_deref().unwrap().to_string(), env)
        })
        .collect();
    let test_sets: Vec<TestSet> = test_sets
        .iter()
        .map(|node| {
            let name = node.attribute("name").unwrap();
            let file = node.attribute("file").unwrap();
            let path = Path::new(catalog)
                .parent()
                .unwrap_or(Path::new("."))
                .join(file);
            println!("Loading {}", file);
            TestSet::new(name, &path)
        })
        .collect();

    println!("{} envs, {} test sets", envs.len(), test_sets.len());

    let smop_runner = Box::new(SmopRunner::new());
    let driver = Driver::new(smop_runner);
    driver.run_tests(&environments, &test_sets);

    if false {
        let package = Package::new();
        let sxd_runner = Box::new(SXDRunner::new(&package));
        let driver = Driver::new(sxd_runner);
        driver.run_tests(&environments, &test_sets);
    }
}
