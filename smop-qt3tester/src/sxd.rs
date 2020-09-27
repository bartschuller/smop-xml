use crate::model::{Assertion, SpecType};
use crate::runner::{Environment, TestError, TestRunner, XpathValue};
use itertools::Itertools;
use sxd_document::Package;
use sxd_xpath::{Context, ExecutionError, Factory, ParserError, Value};

impl From<ParserError> for TestError {
    fn from(pe: ParserError) -> Self {
        TestError::ParseError(Some(pe))
    }
}

impl From<ExecutionError> for TestError {
    fn from(ee: ExecutionError) -> Self {
        TestError::ExecutionError(ee)
    }
}

pub struct SXDRunner<'a> {
    package: &'a Package,
}
impl<'a> SXDRunner<'a> {
    pub fn new(package: &'a Package) -> SXDRunner<'a> {
        let runner = SXDRunner { package };
        runner
    }
    fn xpath_equals(&self, v1: &Value<'a>, v2: &Value<'a>) -> bool {
        match v1 {
            Value::Boolean(b) => v2.boolean() == *b,
            Value::Number(n) => v2.number() == *n,
            Value::String(s) => v2.string() == *s,
            Value::Nodeset(_) => *v1 == *v2,
        }
    }
}
pub struct SXDEnvironment {}
impl SXDEnvironment {
    fn new() -> Self {
        SXDEnvironment {}
    }
}
impl Environment for SXDEnvironment {
    fn set_context_document(&mut self, _file: &str) {
        unimplemented!()
    }

    fn set_namespace(&mut self, _prefix: &str, _uri: &str) {
        unimplemented!()
    }
}
impl XpathValue for Value<'_> {}

fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().join(" ")
}

impl<'a> TestRunner for SXDRunner<'a> {
    type V = Value<'a>;
    type E = SXDEnvironment;

    fn spec_supported(&self, spec: &SpecType) -> bool {
        match spec {
            SpecType::XP20 => false,
            SpecType::XP20Up => false,
            SpecType::XP30 => false,
            SpecType::XP30Up => false,
            SpecType::XP31 => false,
            SpecType::XP31Up => false,
            SpecType::XQ10 => false,
            SpecType::XQ10Up => false,
            SpecType::XQ30 => false,
            SpecType::XQ30Up => false,
            SpecType::XQ31 => false,
            SpecType::XQ31Up => false,
            SpecType::XT30Up => false,
        }
    }

    fn new_environment(&self) -> SXDEnvironment {
        SXDEnvironment::new()
    }

    fn evaluate(&self, _environment: &Self::E, xpath: &str) -> Result<Value<'a>, TestError> {
        let factory = Factory::new();
        let xpath = factory.build(xpath)?.ok_or(TestError::ParseError(None))?;
        let root = self.package.as_document().root();
        let context = Context::new();
        let value = xpath.evaluate(&context, root)?;
        Ok(value)
    }

    fn check(&self, result: &Result<Self::V, TestError>, expected: &Assertion) -> Option<String> {
        match expected {
            Assertion::Assert(_) => Some("wrong".to_string()),
            Assertion::AssertEq(val_string) => match result {
                Ok(v) => {
                    let val = Value::String(val_string.to_string());
                    if self.xpath_equals(v, &val) {
                        None
                    } else {
                        Some(format!(
                            "expected \"{}\", got \"{}\"",
                            val_string,
                            v.string()
                        ))
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertCount(_) => Some("wrong".to_string()),
            Assertion::AssertDeepEq(_) => Some("wrong".to_string()),
            Assertion::AssertPermutation(_) => Some("wrong".to_string()),
            Assertion::AssertXml { .. } => Some("wrong".to_string()),
            Assertion::SerializationMatches { .. } => Some("wrong".to_string()),
            Assertion::AssertSerializationError(_) => Some("wrong".to_string()),
            Assertion::AssertEmpty => Some("wrong".to_string()),
            Assertion::AssertType(_) => Some("wrong".to_string()),
            Assertion::AssertTrue => match result {
                Ok(v) => {
                    if v.boolean() {
                        None
                    } else {
                        Some("expected true, got false".to_string())
                    }
                }
                Err(e) => Some(e.to_string()),
            },
            Assertion::AssertFalse => Some("wrong".to_string()),
            Assertion::AssertStringValue {
                string_value,
                normalize_space,
            } => match result {
                Ok(s) => {
                    let s = s.string();
                    let s = if *normalize_space {
                        normalize_whitespace(&s)
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
            Assertion::Error(code) => match result {
                Ok(_) => Some(format!("Expected error code {}", code)),
                Err(_e) => {
                    // TODO println!("Expected error code {}, got {:?}", code, e);
                    None
                }
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
