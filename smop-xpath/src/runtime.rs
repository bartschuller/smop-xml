use crate::xdm::{QName, Xdm, XdmResult};
use crate::StaticContext;

use im::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Focus<'a, 'input> {
    pub sequence: Xdm<'a, 'input>,
    pub position: usize,
}

#[derive(Clone)]
pub struct DynamicContext<'a, 'input> {
    pub focus: Option<Focus<'a, 'input>>,
    pub static_context: Rc<StaticContext>,
    pub(crate) variables: HashMap<QName, Xdm<'a, 'input>>,
}

impl<'a, 'input> DynamicContext<'a, 'input> {
    pub fn clone_with_focus(&self, sequence: Xdm<'a, 'input>, position: usize) -> Self {
        DynamicContext {
            focus: Some(Focus { sequence, position }),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        }
    }
    pub fn clone_with_variable(&self, qname: QName, value: Xdm<'a, 'input>) -> Self {
        let mut ret = DynamicContext {
            focus: self.focus.clone(),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        };
        ret.set_variable(qname, value);
        ret
    }
    pub fn varref(&self, qname: &QName) -> Option<Xdm<'a, 'input>> {
        self.variables.get(qname).cloned()
    }
    pub fn set_variable(&mut self, qname: QName, value: Xdm<'a, 'input>) {
        self.variables.insert(qname, value);
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
