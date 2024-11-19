use crate::xdm::{Xdm, XdmResult};
use crate::StaticContext;

use im::HashMap;
use std::rc::Rc;
use xot::xmlname::OwnedName;
use xot::{Node};

#[derive(Clone)]
pub struct Focus {
    pub sequence: Xdm,
    /// 0-based position of focus item
    pub position: usize,
    // 0-based index of last item in the focus sequence
    //pub last: usize,
}

impl Focus {
    pub fn last(&self) -> usize {
        usize::MAX - 1
    }
}

#[derive(Clone)]
pub struct DynamicContext {
    pub focus: Option<Focus>,
    pub static_context: Rc<StaticContext>,
    pub(crate) variables: HashMap<OwnedName, Xdm>,
}

impl DynamicContext {
    pub fn with_node(&self, node: Node) -> XdmResult<Self> {
        let xdm = Xdm::Node(node);
        Ok(self.clone_with_focus(xdm, 0))
    }
    pub fn clone_with_focus(&self, sequence: Xdm, position: usize) -> Self {
        DynamicContext {
            focus: Some(Focus { sequence, position }),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        }
    }
    pub fn clone_with_variable(&self, qname: OwnedName, value: Xdm) -> Self {
        let mut ret = DynamicContext {
            focus: self.focus.clone(),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        };
        ret.set_variable(qname, value);
        ret
    }
    pub fn varref(&self, qname: &OwnedName) -> Option<Xdm> {
        self.variables.get(qname).cloned()
    }
    pub fn set_variable(&mut self, qname: OwnedName, value: Xdm) {
        self.variables.insert(qname, value);
    }

    pub fn trace(&self, value: &Xdm) {
        match value.string_joined(self.static_context.as_ref()) {
            Ok(s) => println!("{}", s),
            Err(_) => println!("{:?}", value),
        }
    }
    pub fn trace_label(&self, value: &Xdm, label: &Xdm) -> XdmResult<()> {
        let sc = self.static_context.as_ref();
        let label_string = label.string_joined(sc)?;
        match value.string_joined(sc) {
            Ok(s) => println!("{} {}", label_string, s),
            Err(_) => println!("{} {:?}", label_string, value),
        }
        Ok(())
    }
}

type CompiledExprClosure = dyn for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm>;
pub struct CompiledExpr(Box<CompiledExprClosure>);

impl CompiledExpr {
    pub fn new(
        closure: impl 'static + for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm>,
    ) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute(&self, context: &DynamicContext) -> XdmResult<Xdm> {
        self.0(context)
    }
}
