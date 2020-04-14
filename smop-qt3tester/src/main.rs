extern crate clap;

mod runner;
mod sxd;
use sxd::SXDRunner;
mod driver;
use crate::Assertion::*;
use crate::DependencyType::{
    Calendar, DefaultLanguage, Feature, FormatIntegerSequence, Language, Limits, Spec,
    UnicodeNormalizationForm, UnicodeVersion, XmlVersion, XsdVersion,
};
use crate::SpecType::{
    XP20Up, XP30Up, XP31Up, XQ10Up, XQ30Up, XQ31Up, XT30Up, XP20, XP30, XP31, XQ10, XQ30, XQ31,
};
use clap::{crate_description, crate_name, crate_version, App, Arg};
use driver::Driver;
use roxmltree::Node;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug)]
struct EnvironmentSpec {
    name: Option<String>,
    reference: Option<String>,
}

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
pub struct TestCase {
    name: String,
    description: String,
    environment: Option<EnvironmentSpec>,
    dependencies: Vec<Dependency>,
    test: String,
    result: Assertion,
}
#[derive(Debug)]
pub enum SpecType {
    XP20,
    XP20Up,
    XP30,
    XP30Up,
    XP31,
    XP31Up,
    XQ10,
    XQ10Up,
    XQ30,
    XQ30Up,
    XQ31,
    XQ31Up,
    XT30Up,
}
#[derive(Debug)]
pub enum DependencyType {
    Calendar,
    DefaultLanguage,
    DirectoryAsCollectionUri,
    Feature,
    FormatIntegerSequence,
    Language,
    Limits,
    Spec(Vec<SpecType>),
    UnicodeNormalizationForm,
    UnicodeVersion,
    XmlVersion,
    XsdVersion,
    //    CollectionStability,
    //    SchemaAware,
}
#[derive(Debug)]
pub struct Dependency {}

pub struct TestSet {
    name: String,
    dependencies: Vec<Dependency>,
    environments: HashMap<String, EnvironmentSpec>,
    test_cases: Vec<TestCase>,
}

fn parse_environment(node: &Node) -> EnvironmentSpec {
    let name = node.attribute("name").map(|s| s.to_string());
    let reference = node.attribute("ref").map(|s| s.to_string());
    EnvironmentSpec { name, reference }
}

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
fn find_dependencies(node: &Node) -> Vec<Dependency> {
    node.children()
        .filter_map(|n| {
            if n.is_element() && n.has_tag_name("dependency") {
                Some(create_dependency(&n))
            } else {
                None
            }
        })
        .collect()
}
fn find_environments(node: &Node) -> Vec<EnvironmentSpec> {
    node.children()
        .filter_map(|n| {
            if n.is_element() && n.has_tag_name("environment") {
                Some(parse_environment(&n))
            } else {
                None
            }
        })
        .collect()
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
    let mut environments = find_environments(node);
    assert!(environments.len() <= 1);
    let environment = environments.pop();
    let dependencies: Vec<Dependency> = find_dependencies(node);
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
        environment,
        dependencies,
        test,
        result,
    };
    //println!("{:?}", tc);
    tc
}

fn create_dependency(node: &Node) -> Dependency {
    let typ = node.attribute("type").unwrap_or("");
    let value = node.attribute("value").unwrap_or("");
    let satisfied = boolean_value(node.attribute("satisfied").unwrap_or("true"));
    //println!("{}, {}, {}", typ, value, satisfied);
    let typ = match typ {
        "spec" => Spec(
            value
                .split_ascii_whitespace()
                .map(|s| match s {
                    "XP20" => XP20,
                    "XP20+" => XP20Up,
                    "XP30" => XP30,
                    "XP30+" => XP30Up,
                    "XP31" => XP31,
                    "XP31+" => XP31Up,
                    "XQ10" => XQ10,
                    "XQ10+" => XQ10Up,
                    "XQ30" => XQ30,
                    "XQ30+" => XQ30Up,
                    "XQ31" => XQ31,
                    "XQ31+" => XQ31Up,
                    "XT30+" => XT30Up,
                    &_ => {
                        println!("spec value={}", s);
                        panic!()
                    }
                })
                .collect(),
        ),
        "feature" => Feature,
        "xml-version" => XmlVersion,
        "xsd-version" => XsdVersion,
        "default-language" => DefaultLanguage,
        "language" => Language,
        "limits" => Limits,
        "calendar" => Calendar,
        "format-integer-sequence" => FormatIntegerSequence,
        "unicode-version" => UnicodeVersion,
        "unicode-normalization-form" => UnicodeNormalizationForm,
        &_ => panic!(),
    };
    Dependency {}
}
fn load_test_set(name: &str, file: &Path) -> TestSet {
    let text = std::fs::read_to_string(file).unwrap();
    let doc = roxmltree::Document::parse(&text).unwrap();
    let test_set = doc.root_element();
    let dependencies: Vec<Dependency> = find_dependencies(&test_set);
    let environments: HashMap<String, EnvironmentSpec> = find_environments(&test_set)
        .into_iter()
        .map(|env| (env.name.as_deref().unwrap().to_string(), env))
        .collect();
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
        environments,
        dependencies,
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
    let environments: HashMap<String, EnvironmentSpec> = envs
        .iter()
        .map(|node| {
            let env = parse_environment(node);
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
            load_test_set(name, &path)
        })
        .collect();

    println!("{} envs, {} test sets", envs.len(), test_sets.len());

    let sxd_runner = Box::new(SXDRunner::new());
    let driver = Driver::new(sxd_runner);
    driver.run_tests(&environments, &test_sets);
}
