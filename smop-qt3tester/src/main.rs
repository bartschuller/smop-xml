extern crate clap;

mod runner;
mod sxd;
use sxd::SXDRunner;
mod driver;
use driver::Driver;
use crate::Assertion::*;
use clap::{crate_description, crate_name, crate_version, App, Arg};
use roxmltree::Node;
use std::path::Path;

struct Environment {}

#[derive(Debug)]
pub enum Assertion {
    Assert(String),
    AssertEq(String),
    AssertCount(usize),
    AssertDeepEq(String),
    AssertPermutation(String),
    AssertXml {
        xml_source: String,
        ignore_prefixes: bool,
    },
    SerializationMatches {
        serialization: String,
        flags: String,
    },
    AssertSerializationError(String),
    AssertEmpty,
    AssertType(String),
    AssertTrue,
    AssertFalse,
    AssertStringValue {
        string_value: String,
        normalize_space: bool,
    },
    Error(String),
    AnyOf(Vec<Assertion>),
    AllOf(Vec<Assertion>),
    Not(Box<Assertion>),
}

#[derive(Debug)]
struct TestCase {
    name: String,
    description: String,
    test: String,
    result: Assertion,
}

#[derive(Debug)]
struct Dependency {

}

struct TestSet {
    name: String,
    test_cases: Vec<TestCase>,
}

fn parse_environment() {}

fn boolean_value(s: &str) -> bool {
    s.eq("true") || s.eq("1")
}

fn child_assertions(node: &Node) -> Vec<Assertion> {
    node.children()
        .filter_map(|n| {
            if n.is_element() {
                Some(create_assertion(&n).unwrap())
            } else {
                None
            }
        })
        .collect()
}
fn create_assertion(node: &Node) -> Result<Assertion, String> {
    match node.tag_name().name() {
        "all-of" => Ok(AllOf(child_assertions(node))),
        "assert-eq" => Ok(AssertEq(node.text().unwrap().into())),
        "assert-type" => Ok(AssertType(node.text().unwrap().into())),
        "assert-string-value" => Ok(AssertStringValue {
            string_value: node.text().unwrap_or("").into(),
            normalize_space: node
                .attribute("normalize-space")
                .map_or(false, boolean_value),
        }),
        "error" => Ok(Error(node.attribute("code").unwrap().into())),
        "assert-true" => Ok(AssertTrue),
        "assert-false" => Ok(AssertFalse),
        "any-of" => Ok(AnyOf(child_assertions(node))),
        "assert-empty" => Ok(AssertEmpty),
        "assert-xml" => Ok(AssertXml {
            xml_source: node.text().unwrap_or("").into(),
            ignore_prefixes: node
                .attribute("ignore-prefixes")
                .map_or(false, boolean_value),
        }),
        "assert-deep-eq" => Ok(AssertDeepEq(node.text().unwrap().into())),
        "assert" => Ok(Assert(node.text().unwrap().into())),
        "assert-permutation" => Ok(AssertPermutation(node.text().unwrap().into())),
        "assert-count" => Ok(AssertCount(node.text().unwrap().parse::<usize>().unwrap())),
        "not" => Ok(Not(Box::new(
            node.first_element_child()
                .map(|n| create_assertion(&n))
                .unwrap()
                .unwrap(),
        ))),
        "assert-serialization-error" => Ok(AssertSerializationError(
            node.attribute("code").unwrap().into(),
        )),
        "serialization-matches" => Ok(SerializationMatches {
            serialization: node.text().unwrap_or("").into(),
            flags: node.attribute("flags").unwrap_or("").into(),
        }),
        &_ => Err(format!("Unknown assertion found: {:?}", node)),
    }
}

fn create_test_case(node: &Node, file: &Path) -> TestCase {
    let name = node.attribute("name").unwrap().into();
    let description = node
        .children()
        .find_map(|n| {
            if n.has_tag_name("description") {
                n.text()
            } else {
                None
            }
        })
        .unwrap_or("")
        .into();
    let test = node
        .children()
        .find_map(|n| {
            if n.has_tag_name("test") {
                n.attribute("file").map(|f| "FILE").or(n.text())
            } else {
                None
            }
        })
        .unwrap_or("")
        .into();
    let result = node
        .children()
        .find_map(|n| {
            if n.has_tag_name("result") {
                Some(create_assertion(&n.first_element_child().unwrap()))
            } else {
                None
            }
        })
        .unwrap()
        .unwrap();
    let tc = TestCase {
        name,
        description,
        test,
        result,
    };
    println!("{:?}", tc);
    tc
}

fn load_test_set(name: &str, file: &Path) -> TestSet {
    let text = std::fs::read_to_string(file).unwrap();
    let doc = roxmltree::Document::parse(&text).unwrap();
    let test_set = doc.root_element();
    //let dependency =
    let test_cases: Vec<TestCase> = test_set
        .children()
        .filter_map(|n| {
            if n.is_element() && n.has_tag_name("test-case") {
                Some(create_test_case(&n, &file))
            } else {
                None
            }
        })
        .collect();
    TestSet {
        name: name.into(),
        test_cases,
    }
}

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
            load_test_set(name, &path)
        })
        .collect();

    println!("{} envs, {} test sets", envs.len(), test_sets.len());

    let sxd_runner = SXDRunner::new();
    let driver = Driver::new(Box::new(sxd_runner));
}
