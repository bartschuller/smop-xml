use crate::model::{Assertion, SpecType};
use crate::runner::{Environment, TestError, TestRunner, XpathValue};
use itertools::Itertools;
use smop_xmltree::nod::{Document, QName};
use std::rc::Rc;
use xpath::ast::Comp;
use xpath::runtime::DynamicContext;
use xpath::types::{Item, Occurrence, SequenceType};
use xpath::xdm::{Xdm, XdmError};
use xpath::{StaticContext, Xpath};

impl From<XdmError> for TestError {
    fn from(xe: XdmError) -> Self {
        TestError::ErrorCode(xe.code.name, xe.message)
    }
}

pub struct SmopRunner {
    pub(crate) static_context: Rc<StaticContext>,
    pub(crate) context: DynamicContext,
}
impl SmopRunner {
    pub fn new() -> SmopRunner {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context = static_context.new_dynamic_context();
        let runner = SmopRunner {
            static_context,
            context,
        };
        runner
    }
    fn xpath_equals(&self, v1: &Xdm, v2: &Xdm) -> bool {
        v1.xpath_compare(v2, Comp::EQ).unwrap()
    }
}
pub struct SmopEnvironment {
    static_context: Rc<StaticContext>,
    context: DynamicContext,
}
impl SmopEnvironment {
    fn new() -> SmopEnvironment {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context = static_context.new_dynamic_context();
        SmopEnvironment {
            static_context,
            context,
        }
    }
}
impl Environment for SmopEnvironment {
    fn set_context_document(&mut self, file: &str) {
        let text = std::fs::read_to_string(file).unwrap();
        let xdm = Xdm::Node(Document::parse(text.as_str()).unwrap().root());
        self.context = self.context.clone_with_focus(xdm, 0)
    }
}
impl XpathValue for Xdm {}

fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().join(" ")
}

impl TestRunner for SmopRunner {
    type V = Xdm;
    type E = SmopEnvironment;

    fn spec_supported(&self, spec: &SpecType) -> bool {
        match spec {
            SpecType::XP20 => false,
            SpecType::XP20Up => true,
            SpecType::XP30 => false,
            SpecType::XP30Up => true,
            SpecType::XP31 => true,
            SpecType::XP31Up => true,
            SpecType::XQ10 => false,
            SpecType::XQ10Up => false,
            SpecType::XQ30 => false,
            SpecType::XQ30Up => false,
            SpecType::XQ31 => false,
            SpecType::XQ31Up => false,
            SpecType::XT30Up => false,
        }
    }

    fn new_environment(&self) -> SmopEnvironment {
        SmopEnvironment::new()
    }

    fn evaluate(&self, environment: &SmopEnvironment, xpath: &str) -> Result<Self::V, TestError> {
        let xpath = Xpath::compile(&environment.static_context, xpath)?;
        let result = xpath.evaluate(&environment.context)?;
        Ok(result)
    }

    fn check(&self, result: &Result<Self::V, TestError>, expected: &Assertion) -> Option<String> {
        match expected {
            Assertion::Assert(xpath) => match result {
                Ok(v) => {
                    let result_q = QName::new("result".to_string(), None, None);
                    let mut static_context = (&*self.static_context).clone();
                    static_context.set_variable_type(
                        &result_q,
                        SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
                    );
                    let result_context = self.context.clone_with_variable(result_q, v.clone());
                    let assertion = Xpath::compile(&Rc::new(static_context), xpath)
                        .unwrap()
                        .evaluate(&result_context)
                        .unwrap();
                    if assertion.boolean().unwrap() {
                        None
                    } else {
                        Some(format!(
                            "expected \"{}\" to be true, got \"{}\"",
                            xpath,
                            v.string_joined().unwrap()
                        ))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertEq(val_string) => match result {
                Ok(v) => {
                    let val = Xpath::compile(&self.static_context, val_string)
                        .unwrap()
                        .evaluate(&self.context)
                        .unwrap();
                    if self.xpath_equals(v, &val) {
                        None
                    } else {
                        Some(format!(
                            "expected \"{}\", got \"{}\"",
                            val_string,
                            v.string().unwrap()
                        ))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertCount(count) => match result {
                Ok(v) => {
                    if v.count() == *count {
                        None
                    } else {
                        Some(format!("expected {} items, got {}", *count, v.count()))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertDeepEq(other_string) => match result {
                Ok(v) => {
                    let val = Xpath::compile(&self.static_context, other_string)
                        .unwrap()
                        .evaluate(&self.context)
                        .unwrap();
                    if v.deep_equal(&val) {
                        None
                    } else {
                        Some(format!(
                            "expected \"{}\", got \"{}\"",
                            other_string,
                            v.string_joined().unwrap()
                        ))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertPermutation(_) => Some("wrong".to_string()),
            Assertion::AssertXml {
                xml_source,
                ignore_prefixes: _,
            } => match result {
                Ok(v) => {
                    let source = format!("<XyZzY>{}</XyZzY>", xml_source);
                    let xdm = Xdm::Node(Document::parse(source.as_str()).unwrap().root());
                    let ctx = self.context.clone_with_focus(xdm, 0);
                    let val = Xpath::compile(&self.static_context, "*/child::node()")
                        .unwrap()
                        .evaluate(&ctx)
                        .unwrap();
                    if v.deep_equal(&val) {
                        None
                    } else {
                        Some(format!(
                            "expected {}, got \"{}\"",
                            xml_source,
                            v.string_joined().unwrap()
                        ))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::SerializationMatches { .. } => Some("wrong".to_string()),
            Assertion::AssertSerializationError(_) => Some("wrong".to_string()),
            Assertion::AssertEmpty => match result {
                Ok(Xdm::Sequence(v)) if v.is_empty() => None,
                Ok(x) => Some(format!(
                    "expected an empty sequence, got {}",
                    x.string().unwrap()
                )),
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertType(_) => Some("wrong".to_string()),
            Assertion::AssertTrue => match result {
                Ok(v) => {
                    if v.boolean().unwrap() {
                        None
                    } else {
                        Some("expected true, got false".to_string())
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertFalse => match result {
                Ok(v) => {
                    if !v.boolean().unwrap() {
                        None
                    } else {
                        Some("expected false, got true".to_string())
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertStringValue {
                string_value,
                normalize_space,
            } => match result {
                Ok(s) => {
                    let s = s.string_joined().unwrap();
                    let s = if *normalize_space {
                        normalize_whitespace(s.as_str())
                    } else {
                        s
                    };
                    if s.eq(string_value) {
                        None
                    } else {
                        Some(format!("expected \"{}\", got \"{}\"", string_value, s))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::Error(expected_code) => match result {
                Err(TestError::ErrorCode(code, message)) => {
                    if code == expected_code {
                        None
                    } else {
                        Some(format!(
                            "Expected error code {}, got {} ({})",
                            expected_code, code, message
                        ))
                    }
                }
                _ => Some(format!("Expected error code {}", expected_code)),
            },
            Assertion::AnyOf(v) => {
                let all_checks = v.iter().map(|a| self.check(result, a));
                type B = Vec<Option<String>>;
                let (failures, successes): (B, B) = all_checks.partition(|o| o.is_some());
                if !successes.is_empty() {
                    None
                } else {
                    Some(format!(
                        "all alternatives failed: {}",
                        failures.iter().map(|o| o.as_ref().unwrap()).join(", ")
                    ))
                }
            }
            Assertion::AllOf(v) => {
                let all_checks = v.iter().map(|a| self.check(result, a));
                let mut failures = all_checks.filter_map(|o| o).peekable();
                if failures.peek().is_none() {
                    None
                } else {
                    Some(format!(
                        "one or more alternatives failed: {}",
                        failures.join(", ")
                    ))
                }
            }
            Assertion::Not(_) => Some("wrong".to_string()),
        }
    }
}
