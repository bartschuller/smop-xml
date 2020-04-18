use crate::model::SpecType;
use std::error::Error;
use std::fmt;

pub trait XpathValue {}
pub trait Environment {
    fn bind_value(&mut self, name: &str);
}
#[derive(Debug)]
pub enum TestError {
    Unknown,
    Message(String),
    ParseError(Option<sxd_xpath::ParserError>),
    ExecutionError(sxd_xpath::ExecutionError),
    ErrorCode(String),
}
impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            TestError::Unknown => write!(f, "unknown test error"),
            TestError::Message(ref msg) => write!(f, "test error: {}", msg),
            TestError::ErrorCode(ref code) => write!(f, "XPath error code {}", code),
            TestError::ParseError(Some(ref pe)) => write!(f, "Parse error: {}", pe),
            TestError::ParseError(None) => write!(f, "Parse error"),
            TestError::ExecutionError(ref ee) => write!(f, "Execution error: {}", ee),
        }
    }
}
impl Error for TestError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            TestError::ParseError(Some(ref pe)) => Some(pe),
            TestError::ExecutionError(ref ee) => Some(ee),
            _ => None,
        }
    }
}
pub trait TestRunner {
    type V: XpathValue;
    type E: Environment;
    fn spec_supported(&self, spec: SpecType) -> bool;
    fn new_environment(&self) -> Self::E;
    fn evaluate(&self, xpath: &str) -> Result<Self::V, TestError>;
}
