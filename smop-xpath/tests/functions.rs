use std::rc::Rc;
use smop_xpath::runtime::DynamicContext;
use smop_xpath::xdm::{Xdm, XdmResult};
use smop_xpath::{StaticContext, Xpath};

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

#[test]
fn subsequence1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(
        &static_context,
        r#"let $seq := ("item1", "item2", "item3", "item4", "item5") return fn:subsequence($seq, 4)"#,
    )?;
    let result = xpath.evaluate(&context)?;
    assert_eq!("item4 item5", result.string_joined()?);
    let xpath = Xpath::compile(
        &static_context,
        r#"let $seq := ("item1", "item2", "item3", "item4", "item5") return fn:subsequence($seq, 6)"#,
    )?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(Xdm::EmptySequence, result);
    let xpath = Xpath::compile(
        &static_context,
        r#"let $seq := ("item1", "item2", "item3", "item4", "item5") return fn:subsequence($seq, -5)"#,
    )?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(5, result.count());
    Ok(())
}

#[test]
fn join1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#"fn:string-join(1 to 9)"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!("123456789", result.string()?);
    Ok(())
}

#[test]
fn join2() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let context: DynamicContext = static_context.new_dynamic_context();
    let xpath = Xpath::compile(&static_context, r#"fn:string-join(1 to 9, ', ')"#)?;
    let result = xpath.evaluate(&context)?;
    assert_eq!("1, 2, 3, 4, 5, 6, 7, 8, 9", result.string()?);
    Ok(())
}
