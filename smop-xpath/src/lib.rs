#![warn(clippy::all)]

#[macro_use]
extern crate pest_derive;

pub mod ast;
pub mod context;
mod debugparser;
mod functions;
pub mod parser;
mod runtime;
mod types;
pub mod xdm;
mod xpath_functions_31;
pub use crate::context::StaticContext;
use crate::runtime::{CompiledExpr, DynamicContext};
use crate::xdm::{XdmError, XdmResult};
use xdm::Xdm;

pub struct Xpath(CompiledExpr);

impl<'input, 's_ctx> Xpath
where
    's_ctx: 'input,
{
    pub fn compile(context: &'s_ctx StaticContext, xpath: &'input str) -> XdmResult<Xpath> {
        let expr = context
            .parse(xpath)
            .map_err(|e| XdmError::xqtm("XPST0003", e.to_string().as_str()))?;
        Ok(Xpath(expr.compile(context)))
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
    use crate::{StaticContext, Xpath};
    use rust_decimal::Decimal;
    use std::str::FromStr;

    #[test]
    fn compile1() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "1")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(1));
        Ok(())
    }

    #[test]
    fn compile2() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "1,'two'")?;
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
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "if (1) then 'hello' else 42")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::String("hello".to_string()));
        let xpath = Xpath::compile(&static_context, "if (0) then 'hello' else 42")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(42));
        Ok(())
    }
    #[test]
    fn bool1() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "0")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, false);
        let xpath = Xpath::compile(&static_context, "1")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, true);
        Ok(())
    }
    #[test]
    fn bool2() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "1, 2")?;
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
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "()")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Sequence(vec![]));
        let xpath = Xpath::compile(&static_context, "(3)")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(3));
        let xpath = Xpath::compile(&static_context, "(1, 2)")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result,
            Xdm::Sequence(vec![Xdm::Integer(1), Xdm::Integer(2)])
        );
        let xpath = Xpath::compile(&static_context, "(1, 2, (3, 4))")?;
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
    fn arith1() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, "1 + 2")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(3));
        Ok(())
    }
    #[test]
    fn arith2() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let input = "1 + 0.2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:decimal".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Decimal(Decimal::from_str("1.2").unwrap()));
        Ok(())
    }
    #[test]
    fn arith3() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let input = "0.2 + 3e-2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:anyAtomicType".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Double(0.23_f64));
        Ok(())
    }
    #[test]
    #[ignore]
    fn context1() -> XdmResult<()> {
        let static_context: StaticContext = Default::default();
        let xpath = Xpath::compile(&static_context, ".")?;
        let context: DynamicContext = Default::default();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Sequence(vec![]));
        Ok(())
    }
}
