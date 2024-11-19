use smop_xpath::context::Context;
use smop_xpath::xdm::XdmResult;
use smop_xpath::Xpath;

#[test]
fn context1() -> XdmResult<()> {
    let context = Context::new();
    let xpath = Xpath::compile(&context.static_context, "1");
    let res = xpath?.evaluate(&context.dynamic_context);
    assert_eq!(res?.integer(&context.static_context)?, 1);
    Ok(())
}
