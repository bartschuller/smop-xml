use xpath::context::Context;
use xpath::xdm::XdmResult;
use xpath::Xpath;

#[test]
fn context1() -> XdmResult<()> {
    let context = Context::new();
    let xpath = Xpath::compile(&context.static_context, "1");
    let res = xpath?.evaluate(&context.dynamic_context);
    assert_eq!(res?.integer()?, 1);
    Ok(())
}
