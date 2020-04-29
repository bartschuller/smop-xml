pub mod ast;
mod parser;
pub mod parser3;
mod runtime;
mod xdm;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::runtime::{CompiledExpr, DynamicContext};
use crate::xdm::{XdmError, XdmResult};
use std::error::Error;
use xdm::Xdm;

pub struct Xpath<'a>(CompiledExpr<'a>);

impl<'input> Xpath<'input> {
    pub fn compile(xpath: &'input str) -> XdmResult<Xpath<'input>> {
        let expr = parser3::p3_parse(xpath)
            .map_err(|e| XdmError::xqtm("XPST0003", e.to_string().as_str()))?;
        Ok(Xpath(expr.compile()))
    }

    fn evaluate<'context>(
        &self,
        context: &'context DynamicContext<'context>,
    ) -> XdmResult<Xdm<'context>> {
        self.0.execute(context)
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::xdm::{Xdm, XdmResult};
    use crate::Xpath;

    #[test]
    fn compile1() -> XdmResult<()> {
        let xpath = Xpath::compile("1")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(1));
        Ok(())
    }

    #[test]
    fn compile2() -> XdmResult<()> {
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

    #[test]
    fn if1() -> XdmResult<()> {
        let xpath = Xpath::compile("if (1) then 'hello' else 42")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::String("hello".to_string()));
        Ok(())
    }
    #[test]
    fn bool1() -> XdmResult<()> {
        let xpath = Xpath::compile("1, 2")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        println!("{:?}", result);
        assert_eq!(
            result.boolean().expect_err("expected an error").code,
            "FORG0006"
        );
        Ok(())
    }
}
