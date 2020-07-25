use std::rc::Rc;
use xpath::runtime::DynamicContext;
use xpath::xdm::XdmResult;
use xpath::{StaticContext, Xpath};

#[test]
fn fn_concat1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#"concat("a", "b")"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "ab");
    let xpath = Xpath::compile(&static_context, r#"concat("a", "b", 2)"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.string()?, "ab2");
    let xpath = Xpath::compile(&static_context, r#"concat("a")"#);
    assert!(xpath.is_err());
    Ok(())
}

#[test]
fn nan1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#"not(xs:double("NaN"))"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.boolean()?, true);
    Ok(())
}
