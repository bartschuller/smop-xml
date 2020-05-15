use crate::xdm::{NodeSeq, Xdm, XdmResult};
use roxmltree::Document;

pub struct Focus<'a, 'input> {
    pub sequence: Xdm<'a, 'input>,
    pub position: usize,
}

#[derive(Default)]
pub struct DynamicContext<'a, 'input> {
    pub focus: Option<Focus<'a, 'input>>,
}
impl<'a, 'input> DynamicContext<'a, 'input> {
    pub fn set_context_sequence(&mut self, sequence: Xdm<'a, 'input>, position: usize) {
        self.focus = Some(Focus { sequence, position })
    }
}
pub struct CompiledExpr(
    Box<
        dyn for<'a, 'input, 'context> Fn(
            &'context DynamicContext<'a, 'input>,
        ) -> XdmResult<Xdm<'a, 'input>>,
    >,
);

impl CompiledExpr {
    pub fn new(
        closure: impl 'static
            + for<'a, 'input, 'context> Fn(
                &'context DynamicContext<'a, 'input>,
            ) -> XdmResult<Xdm<'a, 'input>>,
    ) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute<'a, 'input, 'context>(
        &self,
        context: &'context DynamicContext<'a, 'input>,
    ) -> XdmResult<Xdm<'a, 'input>> {
        self.0(context)
    }
}
