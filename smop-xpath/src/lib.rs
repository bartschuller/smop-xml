#![warn(clippy::all)]

#[macro_use]
extern crate pest_derive;

pub mod ast;
pub mod context;
mod debugparser;
mod functions;
pub mod parser;
mod roxml;
pub mod runtime;
mod types;
pub mod xdm;
mod xpath_functions_31;
pub use crate::context::StaticContext;
use crate::runtime::{CompiledExpr, DynamicContext};
use crate::xdm::{XdmError, XdmResult};
use xdm::Xdm;

pub struct Xpath(CompiledExpr);

impl Xpath {
    pub fn compile(context: &StaticContext, xpath: &str) -> XdmResult<Xpath> {
        let expr = context
            .parse(xpath)
            .map_err(|e| XdmError::xqtm("XPST0003", e.to_string().as_str()))?;
        expr.compile(context).map(|compiled| Xpath(compiled))
    }

    pub fn evaluate<'a, 'input, 'context>(
        &self,
        context: &'context DynamicContext<'a, 'input>,
    ) -> XdmResult<Xdm<'a, 'input>> {
        self.0.execute(context)
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::DynamicContext;
    use crate::types::Item;
    use crate::types::{Occurrence, SequenceType};
    use crate::xdm::{NodeSeq, QName, Xdm, XdmResult};
    use crate::{StaticContext, Xpath};
    use roxmltree::Document;
    use rust_decimal::Decimal;
    use std::rc::Rc;
    use std::str::FromStr;

    #[test]
    fn compile1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(1));
        Ok(())
    }

    #[test]
    fn compile2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1,'two'")?;
        let context: DynamicContext = static_context.new_dynamic_context();
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
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "if (1) then 'hello' else 42")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::String("hello".to_string()));
        let xpath = Xpath::compile(&static_context, "if (0) then 'hello' else 42")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(42));
        Ok(())
    }
    #[test]
    fn bool1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "0")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, false);
        let xpath = Xpath::compile(&static_context, "1")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.boolean()?, true);
        Ok(())
    }
    #[test]
    fn bool2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1, 2")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result.boolean().expect_err("expected an error").code,
            "FORG0006"
        );
        Ok(())
    }
    #[test]
    fn parens1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "()")?;
        let context: DynamicContext = static_context.new_dynamic_context();
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
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1 + 2")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(3));
        Ok(())
    }
    #[test]
    fn arith2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "1 + 0.2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:decimal".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Decimal(Decimal::from_str("1.2").unwrap()));
        Ok(())
    }
    #[test]
    fn arith3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "0.2 + 3e-2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:anyAtomicType".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Double(0.23_f64));
        Ok(())
    }
    #[test]
    fn value_compare1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "'a' ge 'b'";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:boolean?".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn roxml1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r##"<root>
            <other stringattr="foo" numattr="42">foo</other>
            <mychild>bar bar</mychild>
            <other stringattr="baz" numattr="0">baz</other>
        </root>"##;
        let rodoc = Document::parse(doc)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let xdm = Xdm::NodeSeq(NodeSeq::RoXml(rodoc.root()));
        let context = context.clone_with_focus(xdm, 0);
        //let xpath = Xpath::compile(&static_context, "/root/mychild/@numattr")?;
        let xpath = Xpath::compile(&static_context, "string-join(child::root/child::mychild)")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("bar bar".to_string()));
        let xpath = Xpath::compile(
            &static_context,
            "child::root/child::other[attribute::stringattr eq 'foo']",
        )?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("foo".to_string()));
        let xpath = Xpath::compile(
            &static_context,
            "child::root/child::other[attribute::stringattr eq 'foo']/attribute::numattr",
        )?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.integer(), Ok(42));
        Ok(())
    }
    #[test]
    fn roxml2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r##"<root>
            <other stringattr="foo" numattr="42">foo</other>
            <mychild>bar bar</mychild>
            <other stringattr="baz" numattr="0">baz</other>
        </root>"##;
        let rodoc = Document::parse(doc)?;

        let context: DynamicContext = static_context.new_dynamic_context();

        let xdm = Xdm::NodeSeq(NodeSeq::RoXml(rodoc.root()));
        let context = context.clone_with_focus(xdm, 0);
        let xpath = Xpath::compile(
            &static_context,
            "(fn:root(self::node()) treat as document-node())/root/other[1]/@numattr",
        )?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("42".to_string()));
        let xpath = Xpath::compile(&static_context, "/root/other[1]/@numattr")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("42".to_string()));
        Ok(())
    }
    #[test]
    fn function1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "boolean(1, 2)");
        assert!(xpath.is_err());
        Ok(())
    }
    #[test]
    fn function2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "not(not(boolean('foo')))")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn instance_of1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1 instance of xs:integer")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn instance_of2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "'foo' instance of xs:integer")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn instance_of3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1 instance of xs:string")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn instance_of4() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "1 instance of xs:decimal")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn var1() -> XdmResult<()> {
        let mut static_context: StaticContext = Default::default();
        let a_type = static_context.schema_type(&static_context.qname("xs", "integer").unwrap())?;
        static_context.set_variable_type(
            QName::new("a".to_string(), None, None),
            SequenceType::Item(Item::AtomicOrUnion(a_type), Occurrence::One),
        );
        let xpath = Xpath::compile(&static_context, "$a")?;
        let static_context = Rc::new(static_context);
        let mut context: DynamicContext = static_context.new_dynamic_context();
        context.set_variable(QName::new("a".to_string(), None, None), Xdm::Integer(42));
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.integer()?, 42);
        Ok(())
    }
    #[test]
    fn for1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "for $i in 10 return $i";
        let expr = static_context.parse(input)?;
        let result = expr.type_(&static_context)?.to_string();
        assert_eq!(result, "xs:integer".to_string());
        Ok(())
    }
}
