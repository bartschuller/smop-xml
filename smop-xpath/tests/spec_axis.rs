use smop_xmltree::nod::Document;
use std::rc::Rc;
use xpath::runtime::DynamicContext;
use xpath::xdm::{NodeSeq, Xdm, XdmResult};
use xpath::{StaticContext, Xpath};

static TREE_REPEAT: &str = include_str!("TreeRepeat.xml");
static TREE_COMPASS: &str = include_str!("TreeCompass.xml");

#[test]
fn axis1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let xdm = Xdm::NodeSeq(NodeSeq::RoXml(Document::parse(TREE_REPEAT).unwrap().root()));
    let context: DynamicContext = static_context
        .new_dynamic_context()
        .clone_with_focus(xdm, 0);
    let xpath = Xpath::compile(
        &static_context,
        "count(//center/descendant-or-self::center)",
    )?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.integer()?, 9);
    Ok(())
}

#[test]
fn axis2() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let xdm = Xdm::NodeSeq(NodeSeq::RoXml(
        Document::parse(TREE_COMPASS).unwrap().root(),
    ));
    let context: DynamicContext = static_context
        .new_dynamic_context()
        .clone_with_focus(xdm, 0);
    let xpath = Xpath::compile(
        &static_context,
        "fn:count(//center/@center-attr-3/descendant-or-self::node())",
    )?;
    let result = xpath.evaluate(&context)?;
    assert_eq!(result.integer()?, 1);
    Ok(())
}
