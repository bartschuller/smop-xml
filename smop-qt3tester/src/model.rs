use roxmltree::Node;
use std::collections::HashMap;
use std::path::Path;
use Dependency::{
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
    pub(crate) sources: Vec<Source>,
}
impl EnvironmentSpec {
    pub(crate) fn new(node: &Node, file: &Path) -> Self {
        let name = node.attribute("name").map(|s| s.to_string());
        let reference = node.attribute("ref").map(|s| s.to_string());
        let sources = Source::find_sources(node, file);
        EnvironmentSpec {
            name,
            reference,
            sources,
        }
    }
    pub(crate) fn find_environments(node: &Node, file: &Path) -> Vec<EnvironmentSpec> {
        node.children()
            .filter_map(|n| {
                if n.is_element() && n.has_tag_name("environment") {
                    Some(EnvironmentSpec::new(&n, file))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct Source {
    pub(crate) role: Option<String>,
    pub(crate) file: String,
}

impl Source {
    pub(crate) fn new(node: &Node, base_file: &Path) -> Self {
        let file = node.attribute("file").map(|s| s.to_string()).unwrap();
        let path = base_file.parent().unwrap_or(Path::new(".")).join(file);
        let role = node.attribute("role").map(|s| s.to_string());
        Source {
            role,
            file: path.to_string_lossy().to_string(),
        }
    }
    pub(crate) fn find_sources(node: &Node, file: &Path) -> Vec<Source> {
        node.children()
            .filter_map(|n| {
                if n.is_element() && n.has_tag_name("source") {
                    Some(Source::new(&n, file))
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
            "all-of" => Assertion::AllOf(Self::child_assertions(node)),
            "assert-eq" => Assertion::AssertEq(node.text().unwrap().into()),
            "assert-type" => Assertion::AssertType(node.text().unwrap().into()),
            "assert-string-value" => Assertion::AssertStringValue {
                string_value: node.text().unwrap_or("").into(),
                normalize_space: node
                    .attribute("normalize-space")
                    .map_or(false, boolean_value),
            },
            "error" => Assertion::Error(node.attribute("code").unwrap().into()),
            "assert-true" => Assertion::AssertTrue,
            "assert-false" => Assertion::AssertFalse,
            "any-of" => Assertion::AnyOf(Self::child_assertions(node)),
            "assert-empty" => Assertion::AssertEmpty,
            "assert-xml" => Assertion::AssertXml {
                xml_source: node.text().unwrap_or("").into(),
                ignore_prefixes: node
                    .attribute("ignore-prefixes")
                    .map_or(false, boolean_value),
            },
            "assert-deep-eq" => Assertion::AssertDeepEq(node.text().unwrap().into()),
            "assert" => Assertion::Assert(node.text().unwrap().into()),
            "assert-permutation" => Assertion::AssertPermutation(node.text().unwrap().into()),
            "assert-count" => {
                Assertion::AssertCount(node.text().unwrap().parse::<usize>().unwrap())
            }
            "not" => Assertion::Not(Box::new(
                node.first_element_child().map(|n| Self::new(&n)).unwrap(),
            )),
            "assert-serialization-error" => {
                Assertion::AssertSerializationError(node.attribute("code").unwrap().into())
            }
            "serialization-matches" => Assertion::SerializationMatches {
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
    pub(crate) dependencies: Vec<Dependency>,
    pub(crate) test: String,
    pub(crate) result: Assertion,
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
        let mut environments = EnvironmentSpec::find_environments(node, file);
        assert!(environments.len() <= 1);
        let environment = environments.pop();
        let dependencies: Vec<Dependency> = Dependency::find_dependencies(node);
        let test = node
            .children()
            .find_map(|n| {
                if n.has_tag_name("test") {
                    n.attribute("file").map(|_f| "FILE").or(n.text())
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
pub enum Dependency {
    Calendar,
    DefaultLanguage,
    #[allow(dead_code)]
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
    Not(Box<Dependency>),
}

fn boolean_value(s: &str) -> bool {
    s.eq("true") || s.eq("1")
}

impl Dependency {
    pub(crate) fn new(node: &Node) -> Self {
        let typ = node.attribute("type").unwrap_or("");
        let value = node.attribute("value").unwrap_or("");
        let satisfied = boolean_value(node.attribute("satisfied").unwrap_or("true"));
        //println!("{}, {}, {}", typ, value, satisfied);
        let base = match typ {
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
        if satisfied {
            base
        } else {
            Dependency::Not(Box::new(base))
        }
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
    #[allow(dead_code)]
    pub(crate) name: String,
    pub(crate) dependencies: Vec<Dependency>,
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
            EnvironmentSpec::find_environments(&test_set, file)
                .into_iter()
                .map(|env| (env.name.as_deref().unwrap().to_string(), env))
                .collect();
        println!("Envs: {:?}", environments);
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
