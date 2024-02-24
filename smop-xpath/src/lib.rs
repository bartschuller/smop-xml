#![warn(clippy::all)]

#[macro_use]
extern crate lazy_static;

pub mod ast;
mod compiler;
pub mod context;
mod functions;
pub mod parser;
pub mod runtime;
mod smop_xmltree;
mod typer;
pub mod types;
pub mod xdm;
mod xpath_functions_31;

pub use crate::context::StaticContext;
use crate::runtime::{CompiledExpr, DynamicContext};
use crate::xdm::XdmResult;
use std::rc::Rc;
use xdm::Xdm;

pub struct Xpath(CompiledExpr);

impl Xpath {
    pub fn compile(context: &Rc<StaticContext>, xpath: &str) -> XdmResult<Xpath> {
        let expr = context.parse(xpath)?;
        expr.type_(Rc::clone(context))?.compile().map(Xpath)
    }

    pub fn evaluate(&self, context: &DynamicContext) -> XdmResult<Xdm> {
        self.0.execute(context)
    }
}

#[cfg(test)]
mod tests {
    use crate::context::ExpandedName;
    use crate::runtime::DynamicContext;
    use crate::types::Item;
    use crate::types::{Occurrence, SequenceType};
    use crate::xdm::{Xdm, XdmResult};
    use crate::{StaticContext, Xpath};
    use rust_decimal::Decimal;
    use smop_xmltree::nod::{Document, QName};
    use std::rc::Rc;
    use std::str::FromStr;

    #[test]
    fn compile1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "1")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(1));
        Ok(())
    }

    #[test]
    fn compile2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "1,'two'")?;
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
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "if (1) then 'hello' else 42")?;
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
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "0")?;
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        let xpath = Xpath::compile(&static_context, "1")?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn bool2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "1, 2")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(
            result.boolean().expect_err("expected an error").code,
            ExpandedName::new("http://www.w3.org/2005/xqt-errors", "FORG0006")
        );
        Ok(())
    }
    #[test]
    fn parens1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "()")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::EmptySequence);
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
        let context: DynamicContext = static_context.new_dynamic_context();
        let xpath = Xpath::compile(&static_context, "1 + 2")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Integer(3));
        Ok(())
    }
    #[test]
    fn arith2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "1 + 0.2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(Rc::clone(&static_context))?.t().0.to_string();
        assert_eq!(result, "xs:decimal".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Decimal(Decimal::from_str("1.2").unwrap()));
        Ok(())
    }
    #[test]
    fn arith3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "0.2 + 3e-2";
        let expr = static_context.parse(input)?;
        let result = expr.type_(Rc::clone(&static_context))?.t().0.to_string();
        assert_eq!(result, "xs:anyAtomicType".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result, Xdm::Double(0.23_f64));
        Ok(())
    }
    #[test]
    fn value_compare1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "'a' ge 'b'";
        let expr = static_context.parse(input)?;
        let result = expr.type_(Rc::clone(&static_context))?.t().0.to_string();
        assert_eq!(result, "xs:boolean?".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn value_compare2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "1 eq 1";
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn general_compare1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "'a' >= 'b'";
        let expr = static_context.parse(input)?;
        let result = expr.type_(Rc::clone(&static_context))?.t().0.to_string();
        assert_eq!(result, "xs:boolean".to_string());
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn general_compare2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let context: DynamicContext = static_context.new_dynamic_context();
        let input = "(1, 2) > (1, 1)";
        let xpath = Xpath::compile(&static_context, input)?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
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
        let xdm = Xdm::Node(rodoc.root());
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
        let context: DynamicContext = static_context.new_dynamic_context().with_xml(doc)?;
        let xpath = Xpath::compile(
            &static_context,
            "(fn:root(self::node()) treat as document-node())/root/other[1]/@numattr",
        )?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("42".to_string()));
        let xpath = Xpath::compile(&static_context, "/root/other[1]/@numattr")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string(), Ok("42".to_string()));
        let xpath = Xpath::compile(&static_context, "count(//self::element())")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.integer(), Ok(4));
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
            ("", "a"),
            SequenceType::Item(Item::AtomicOrUnion(a_type), Occurrence::One),
        );
        let static_context = Rc::new(static_context);
        let xpath = Xpath::compile(&static_context, "$a")?;
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
        let result = expr.type_(static_context)?.t().0.to_string();
        assert_eq!(result, "xs:integer".to_string());
        Ok(())
    }
    #[test]
    fn let1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "let $i := 'a' return $i";
        let expr = static_context.parse(input)?;
        let result = expr.type_(static_context)?.t().0.to_string();
        assert_eq!(result, "xs:string".to_string());
        Ok(())
    }
    #[test]
    fn union1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = "a union b";
        let expr = static_context.parse(input)?;
        let result = expr.type_(static_context)?.t().0.to_string();
        assert_eq!(result, "node()*".to_string());
        Ok(())
    }
    #[test]
    fn union2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let xpath = Xpath::compile(&static_context, "//b//e | //a//e")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string_joined()?.as_str(), "1 2");
        Ok(())
    }
    #[test]
    fn for2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let xpath = Xpath::compile(&static_context, "for $h in (/r) return $h//e")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string_joined()?.as_str(), "1 2 3");
        Ok(())
    }
    #[test]
    fn position1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let xpath = Xpath::compile(&static_context, "//e/position()")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string_joined()?.as_str(), "1 2 3");
        Ok(())
    }
    #[test]
    fn path_pred1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e><e>4</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let xpath = Xpath::compile(&static_context, "/r/e[position() eq 2]")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string()?.as_str(), "4");
        let xpath = Xpath::compile(&static_context, "/r/e[1]")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string()?.as_str(), "3");
        let xpath = Xpath::compile(&static_context, "/descendant::e[2]")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string()?.as_str(), "2");
        let xpath = Xpath::compile(&static_context, "//e[2]")?;
        let result = xpath.evaluate(&context)?;
        assert_eq!(result.string()?.as_str(), "4");
        Ok(())
    }

    #[test]
    fn arrow1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let xpath = Xpath::compile(&static_context, "'3' => concat('4') => concat('5', '6')")?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!("3456", result.string()?);
        Ok(())
    }
    #[test]
    fn range1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"string-join(1 to 5)"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!("12345", result.string()?);
        Ok(())
    }
    #[test]
    fn simple_map1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"string-join((1 to 5)!"*")"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!("*****", result.string()?);
        Ok(())
    }
    #[test]
    fn simple_map2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"2!(.*.)"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!(4, result.integer()?);
        Ok(())
    }
    #[test]
    fn simple_map3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"(1 to 10)!(.*.)"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert_eq!("1 4 9 16 25 36 49 64 81 100", result.string_joined()?);
        Ok(())
    }
    #[test]
    fn node_compare1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"/r/a/e is (//e)[1]"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn node_compare2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"/r/a/e << (//e)[2]"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn node_compare3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"//e[.=3] >> (//e)[2]"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let doc = r#"<r><a><e>1</e><b><e>2</e></b></a><e>3</e></r>"#;
        let context = static_context.new_dynamic_context().with_xml(doc)?;
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn quant1() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"some $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
    #[test]
    fn quant2() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"every $x in (1, 2, 3), $y in (2, 3, 4) satisfies $x + $y = 4"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(!result.boolean()?);
        Ok(())
    }
    #[test]
    fn quant3() -> XdmResult<()> {
        let static_context: Rc<StaticContext> = Rc::new(Default::default());
        let input = r#"some $x in (1, 2, 3), $y in ($x, 3, 4) satisfies $x + $y = 4"#;
        let xpath = Xpath::compile(&static_context, input)?;
        let context: DynamicContext = static_context.new_dynamic_context();
        let result = xpath.evaluate(&context)?;
        assert!(result.boolean()?);
        Ok(())
    }
}
