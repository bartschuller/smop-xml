use crate::runtime::DynamicContext;
use crate::types::SequenceType;
use crate::xdm::{Xdm, XdmResult};
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct FunctionKey {
    pub name: String,
    pub ns: String,
    pub arity: usize,
}

#[derive(Clone)]
pub struct Function {
    pub args: Vec<SequenceType>,
    pub type_: SequenceType,
    pub code: fn() -> CompiledFunction,
}

#[derive(Clone)]
pub struct CompiledFunction(
    Rc<dyn for<'context> Fn(&'context DynamicContext, Vec<Xdm>) -> XdmResult<Xdm>>,
);

impl CompiledFunction {
    pub fn new(
        closure: impl 'static + for<'context> Fn(&'context DynamicContext, Vec<Xdm>) -> XdmResult<Xdm>,
    ) -> Self {
        CompiledFunction(Rc::new(closure))
    }
    pub fn execute(&self, context: &DynamicContext, args: Vec<Xdm>) -> XdmResult<Xdm> {
        self.0(context, args)
    }
}
