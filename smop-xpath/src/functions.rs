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
    Rc<
        dyn for<'a, 'input, 'context> Fn(
            &'context DynamicContext,
            Vec<Xdm<'a, 'input>>,
        ) -> XdmResult<Xdm<'a, 'input>>,
    >,
);

impl CompiledFunction {
    pub fn new(
        closure: impl 'static
            + for<'a, 'input, 'context> Fn(
                &'context DynamicContext,
                Vec<Xdm<'a, 'input>>,
            ) -> XdmResult<Xdm<'a, 'input>>,
    ) -> Self {
        CompiledFunction(Rc::new(closure))
    }
    pub fn execute<'a, 'input: 'a, 'context>(
        &self,
        context: &'context DynamicContext<'a, 'input>,
        args: Vec<Xdm<'a, 'input>>,
    ) -> XdmResult<Xdm<'a, 'input>> {
        self.0(context, args)
    }
}
