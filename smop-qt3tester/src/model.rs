use roxmltree::Node;
use std::collections::HashMap;
use std::path::Path;
use Assertion::*;
use DependencyType::{
    Calendar, DefaultLanguage, Feature, FormatIntegerSequence, Language, Limits, Spec,
    UnicodeNormalizationForm, UnicodeVersion, XmlVersion, XsdVersion,
};
use SpecType::{
    XP20Up, XP30Up, XP31Up, XQ10Up, XQ30Up, XQ31Up, XT30Up, XP20, XP30, XP31, XQ10, XQ30, XQ31,
};

#[derive(Debug)]
pub struct EnvironmentSpec {
    pub(crate) name: Option<String>,
    pub(crate) reference: Option<String>,
}
impl EnvironmentSpec {
    pub(crate) fn new(node: &Node) -> Self {
        let name = node.attribute("name").map(|s| s.to_string());
        let reference = node.attribute("ref").map(|s| s.to_string());
        EnvironmentSpec { name, reference }
    }
    pub(crate) fn find_environments(node: &Node) -> Vec<EnvironmentSpec> {
        node.children()
            .filter_map(|n| {
                if n.is_element() && n.has_tag_name("environment") {
                    Some(EnvironmentSpec::new(&n))
                } else {
                    None
                }
            })
            .collect()
    }
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

impl Assertion {
    pub(crate) fn new(node: &Node) -> Self {
        match node.tag_name().name() {
            "all-of" => AllOf(Self::child_assertions(node)),
            "assert-eq" => AssertEq(node.text().unwrap().into()),
            "assert-type" => AssertType(node.text().unwrap().into()),
            "assert-string-value" => AssertStringValue {
                string_value: node.text().unwrap_or("").into(),
                normalize_space: node
                    .attribute("normalize-space")
                    .map_or(false, boolean_value),
            },
            "error" => Error(node.attribute("code").unwrap().into()),
            "assert-true" => AssertTrue,
            "assert-false" => AssertFalse,
            "any-of" => AnyOf(Self::child_assertions(node)),
            "assert-empty" => AssertEmpty,
            "assert-xml" => AssertXml {
                xml_source: node.text().unwrap_or("").into(),
                ignore_prefixes: node
                    .attribute("ignore-prefixes")
                    .map_or(false, boolean_value),
            },
            "assert-deep-eq" => AssertDeepEq(node.text().unwrap().into()),
            "assert" => Assert(node.text().unwrap().into()),
            "assert-permutation" => AssertPermutation(node.text().unwrap().into()),
            "assert-count" => AssertCount(node.text().unwrap().parse::<usize>().unwrap()),
            "not" => Not(Box::new(
                node.first_element_child().map(|n| Self::new(&n)).unwrap(),
            )),
            "assert-serialization-error" => {
                AssertSerializationError(node.attribute("code").unwrap().into())
            }
            "serialization-matches" => SerializationMatches {
                serialization: node.text().unwrap_or("").into(),
                flags: node.attribute("flags").unwrap_or("").into(),
            },
            &_ => {
                println!("Unknown assertion: {:?}", node);
                panic!("Unknown assertion")
            }
        }
    }

    fn child_assertions(node: &Node) -> Vec<Self> {
        node.children()
            .filter_map(|n| {
                if n.is_element() {
                    Some(Self::new(&n))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct TestCase {
    pub(crate) name: String,
    description: String,
    pub(crate) environment: Option<EnvironmentSpec>,
    dependencies: Vec<Dependency>,
    pub(crate) test: String,
    result: Assertion,
}

impl TestCase {
    fn new(node: &Node, file: &Path) -> Self {
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
        let mut environments = EnvironmentSpec::find_environments(node);
        assert!(environments.len() <= 1);
        let environment = environments.pop();
        let dependencies: Vec<Dependency> = Dependency::find_dependencies(node);
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
                    Some(Assertion::new(&n.first_element_child().unwrap()))
                } else {
                    None
                }
            })
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

fn boolean_value(s: &str) -> bool {
    s.eq("true") || s.eq("1")
}

impl Dependency {
    pub(crate) fn new(node: &Node) -> Self {
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

    pub(crate) fn find_dependencies(node: &Node) -> Vec<Self> {
        node.children()
            .filter_map(|n| {
                if n.is_element() && n.has_tag_name("dependency") {
                    Some(Self::new(&n))
                } else {
                    None
                }
            })
            .collect()
    }
}
pub struct TestSet {
    name: String,
    dependencies: Vec<Dependency>,
    pub(crate) environments: HashMap<String, EnvironmentSpec>,
    pub(crate) test_cases: Vec<TestCase>,
}

impl TestSet {
    pub(crate) fn new(name: &str, file: &Path) -> Self {
        let text = std::fs::read_to_string(file).unwrap();
        let doc = roxmltree::Document::parse(&text).unwrap();
        let test_set = doc.root_element();
        let dependencies: Vec<Dependency> = Dependency::find_dependencies(&test_set);
        let environments: HashMap<String, EnvironmentSpec> =
            EnvironmentSpec::find_environments(&test_set)
                .into_iter()
                .map(|env| (env.name.as_deref().unwrap().to_string(), env))
                .collect();
        let test_cases: Vec<TestCase> = test_set
            .children()
            .filter_map(|n| {
                if n.is_element() && n.has_tag_name("test-case") {
                    Some(TestCase::new(&n, &file))
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
}
