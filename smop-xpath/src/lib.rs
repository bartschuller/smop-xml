mod ast;
mod parser;
mod runtime;
mod xdm;

use xdm::XdmSequence;

pub struct XPath {}
#[derive(Debug)]
enum CompileError {
    ParseError(String),
}
#[derive(Debug)]
enum RuntimeError {
    Oops,
}
impl XPath {
    fn compile(xpath: &str) -> Result<Self, CompileError> {
        Ok(XPath {})
    }

    fn evaluate(&self) -> Result<XdmSequence, RuntimeError> {
        Err(RuntimeError::Oops)
    }
}
