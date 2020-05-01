use crate::xdm::{Xdm, XdmResult};

pub struct Focus<'a> {
    context: &'a Xdm<'a>,
    position: usize,
}

#[derive(Default)]
pub struct DynamicContext<'a> {
    pub focus: Option<Focus<'a>>,
}

pub struct CompiledExpr<'a>(
    Box<dyn 'a + for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm<'context>>>,
);

impl<'a> CompiledExpr<'a> {
    pub fn new(
        closure: impl 'a + for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm<'context>>,
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
