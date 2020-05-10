use crate::xdm::{Node, Xdm, XdmResult};
use roxmltree::Document;

pub struct Focus<'a> {
    pub sequence: Xdm<'a>,
    pub position: usize,
}

#[derive(Default)]
pub struct DynamicContext<'a> {
    pub focus: Option<Focus<'a>>,
}
impl<'a> DynamicContext<'a> {
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
