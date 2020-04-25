use crate::xdm::Xdm;

pub struct Focus<'a> {
    context: &'a Xdm<'a>,
    position: usize,
}

#[derive(Default)]
pub struct DynamicContext<'a> {
    focus: Option<Focus<'a>>,
}
pub struct CompiledExpr<'a>(
    Box<dyn 'a + for<'context> Fn(&'context DynamicContext) -> Xdm<'context>>,
);

impl<'a> CompiledExpr<'a> {
    pub fn new(
        closure: impl 'a + for<'context> Fn(&'context DynamicContext) -> Xdm<'context>,
    ) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute<'context>(&self, context: &'context DynamicContext<'context>) -> Xdm<'context> {
        self.0(context)
    }
}
