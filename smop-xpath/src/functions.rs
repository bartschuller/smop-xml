use crate::runtime::DynamicContext;
use crate::types::SequenceType;
use crate::xdm::{Xdm, XdmResult};

#[derive(Eq, PartialEq, Hash)]
pub struct FunctionKey {
    pub name: String,
    pub ns: String,
    pub arity: usize,
}

pub struct Function {
    pub args: Vec<SequenceType>,
    pub type_: SequenceType,
    pub code: fn() -> CompiledFunction,
}

pub struct CompiledFunction(
    Box<
        dyn for<'context> Fn(
            &'context DynamicContext,
            Vec<Xdm<'context>>,
        ) -> XdmResult<Xdm<'context>>,
    >,
);

impl<'a> CompiledFunction {
    pub fn new(
        closure: impl 'static
            + for<'context> Fn(
                &'context DynamicContext,
                Vec<Xdm<'context>>,
            ) -> XdmResult<Xdm<'context>>,
    ) -> Self {
        CompiledFunction(Box::new(closure))
    }
    pub fn execute<'context>(
        &self,
        context: &'context DynamicContext<'context>,
        args: Vec<Xdm<'context>>,
    ) -> XdmResult<Xdm<'context>> {
        self.0(context, args)
    }
}
