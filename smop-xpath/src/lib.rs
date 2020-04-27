pub mod ast;
mod parser;
pub mod parser3;
mod runtime;
mod xdm;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser2);

extern crate pest;
#[macro_use]
extern crate pest_derive;

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

    #[test]
    fn compile1() -> Result<(), XpathError> {
        let xpath = Xpath::compile("1")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(1));
        Ok(())
    }

    #[test]
    fn compile2() -> Result<(), XpathError> {
        let xpath = Xpath::compile("1,'two'")?;
        let context: DynamicContext = Default::default();
        let result1 = xpath.evaluate(&context)?;
        assert_eq!(
            result1,
            Xdm::Sequence(vec![Xdm::Integer(1), Xdm::String("two".to_string())])
        );
        let result2 = xpath.evaluate(&context)?;
        assert_eq!(result1, result2);
        Ok(())
    }
}
