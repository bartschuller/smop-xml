use crate::xdm::XdmSequence;

struct Focus<'a> {
    context: &'a XdmSequence,
    position: usize,
}
struct DynamicContext<'a> {
    focus: Option<Focus<'a>>,
}
pub struct CompiledExpr<'a>(Box<dyn 'a + Fn(&'a DynamicContext) -> XdmSequence>);

impl<'a> CompiledExpr<'a> {
    fn new(closure: impl 'a + Fn(&'a DynamicContext) -> XdmSequence) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute(&self, context: &'a DynamicContext) -> XdmSequence {
        self.0(context)
    }
}
