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
        CompiledFunction(Box::new(closure))
    }
    pub fn execute<'a, 'input: 'a, 'context>(
        &self,
        context: &'context DynamicContext<'a, 'input>,
        args: Vec<Xdm<'a, 'input>>,
    ) -> XdmResult<Xdm<'a, 'input>> {
        self.0(context, args)
    }
}
