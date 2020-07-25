use smop_xmltree::nod::Document;
use std::rc::Rc;
use xpath::runtime::DynamicContext;
use xpath::xdm::{NodeSeq, Xdm, XdmResult};
use xpath::{StaticContext, Xpath};

static TREE_REPEAT: &str = include_str!("TreeRepeat.xml");

#[test]
#[ignore]
fn axis1() -> XdmResult<()> {
    let static_context: Rc<StaticContext> = Rc::new(Default::default());
    let xdm = Xdm::NodeSeq(NodeSeq::RoXml(Document::parse(TREE_REPEAT).unwrap().root()));
    let context: DynamicContext = static_context
        .new_dynamic_context()
        .clone_with_focus(xdm, 0);
    let xpath = Xpath::compile(&static_context, "//center/descendant-or-self::center")?;
    let result = xpath.evaluate(&context)?;
    println!("string_joined: {}", result.string_joined()?);
    assert_eq!(result.integer()?, 9);
    Ok(())
}
