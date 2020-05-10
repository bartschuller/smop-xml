use crate::xdm::{Node, Xdm, XdmResult};
use owning_ref::OwningHandle;
use roxmltree::Document;

pub struct Focus<'a> {
    sequence: Xdm<'a>,
    position: usize,
}

#[derive(Default)]
pub struct DynamicContext<'a> {
    pub focus: Option<Focus<'a>>,
}
impl<'a> DynamicContext<'a> {
    pub unsafe fn set_document(&mut self, text: &'a str) -> XdmResult<()> {
        let rodoc = Box::new(Document::parse(text)?);
        let xdm = Xdm::Node(Node::RoXml(OwningHandle::new_with_fn(rodoc, |rdref| {
            Box::new(rdref.as_ref().unwrap().root())
        })));
        self.set_context_sequence(xdm, 0);
        Ok(())
    }
    pub fn set_context_sequence(&mut self, sequence: Xdm<'a>, position: usize) {
        self.focus = Some(Focus { sequence, position })
    }
}
pub struct CompiledExpr(
    Box<dyn for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm<'context>>>,
);

impl<'a> CompiledExpr {
    pub fn new(
        closure: impl 'static + for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm<'context>>,
    ) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute<'context>(
        &self,
        context: &'context DynamicContext<'context>,
    ) -> XdmResult<Xdm<'context>> {
        self.0(context)
    }
}
