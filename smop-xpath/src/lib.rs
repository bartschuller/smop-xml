mod ast;
mod parser;
mod runtime;
mod xdm;

use crate::runtime::{CompiledExpr, DynamicContext};
use xdm::Xdm;

pub struct Xpath<'a>(CompiledExpr<'a>);

#[derive(Debug)]
pub enum XpathError {
    ParseError(String),
}
impl<'a> Xpath<'a> {
    pub fn compile(xpath: &str) -> Result<Xpath<'a>, XpathError> {
        let expr = parser::parse(xpath).map_err(|e| XpathError::ParseError(e))?;
        Ok(Xpath(expr.compile()))
    }

    fn evaluate<'context>(
        &self,
        context: &'context DynamicContext<'context>,
    ) -> Result<Xdm<'context>, XpathError> {
        Ok(self.0.execute(context))
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::Xdm;
    use crate::{Xpath, XpathError};
    use std::mem;

    #[test]
    fn compile1() -> Result<(), XpathError> {
        let xpath = Xpath::compile("1")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(2));
        Ok(())
    }
}
