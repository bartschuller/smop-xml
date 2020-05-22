use crate::xdm::{NodeSeq, Xdm, XdmResult};
use crate::StaticContext;
use roxmltree::Document;
use std::rc::Rc;

pub struct Focus<'a, 'input> {
    pub sequence: Xdm<'a, 'input>,
    pub position: usize,
}

pub struct DynamicContext<'a, 'input> {
    pub focus: Option<Focus<'a, 'input>>,
    pub static_context: Rc<StaticContext>,
}
impl<'a, 'input> DynamicContext<'a, 'input> {
    pub fn clone_with_focus(&self, sequence: Xdm<'a, 'input>, position: usize) -> Self {
        DynamicContext {
            focus: Some(Focus { sequence, position }),
            static_context: self.static_context.clone(),
        }
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
