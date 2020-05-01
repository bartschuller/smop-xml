pub mod ast;
pub mod parser;
mod runtime;
mod xdm;

use crate::runtime::{CompiledExpr, DynamicContext};
use crate::xdm::{XdmError, XdmResult};
use xdm::Xdm;

pub struct Xpath<'a>(CompiledExpr<'a>);

impl<'input> Xpath<'input> {
    pub fn compile(xpath: &'input str) -> XdmResult<Xpath<'input>> {
        let expr = parser::p3_parse(xpath)
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
        let xpath = Xpath::compile("if (0) then 'hello' else 42")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(42));
        Ok(())
    }
    #[test]
    fn bool1() -> XdmResult<()> {
        let xpath = Xpath::compile("0")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, false);
        let xpath = Xpath::compile("1")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, true);
        Ok(())
    }
    #[test]
    fn bool2() -> XdmResult<()> {
        let xpath = Xpath::compile("1, 2")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result.boolean().expect_err("expected an error").code,
            "FORG0006"
        );
        Ok(())
    }
    #[test]
    fn parens1() -> XdmResult<()> {
        let xpath = Xpath::compile("()")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Sequence(vec![]));
        let xpath = Xpath::compile("(3)")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(3));
        let xpath = Xpath::compile("(1, 2)")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result,
            Xdm::Sequence(vec![Xdm::Integer(1), Xdm::Integer(2)])
        );
        let xpath = Xpath::compile("(1, 2, (3, 4))")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result,
            Xdm::Sequence(vec![
                Xdm::Integer(1),
                Xdm::Integer(2),
                Xdm::Integer(3),
                Xdm::Integer(4)
            ])
        );
        Ok(())
    }
    #[test]
    #[ignore]
    fn context1() -> XdmResult<()> {
        let xpath = Xpath::compile(".")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Sequence(vec![]));
        Ok(())
    }
}
