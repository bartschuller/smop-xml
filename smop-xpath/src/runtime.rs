use crate::xdm::{NodeSeq, Xdm, XdmResult};
use crate::StaticContext;

use im::HashMap;
use smop_xmltree::nod::{Document, QName};
use std::rc::Rc;

#[derive(Clone)]
pub struct Focus {
    pub sequence: Xdm,
    pub position: usize,
}

#[derive(Clone)]
pub struct DynamicContext {
    pub focus: Option<Focus>,
    pub static_context: Rc<StaticContext>,
    pub(crate) variables: HashMap<QName, Xdm>,
}

impl DynamicContext {
    pub fn with_xml(&self, xml: &str) -> XdmResult<Self> {
        let doc = Document::parse(xml)?;
        let xdm = Xdm::NodeSeq(NodeSeq::RoXml(doc.root()));
        Ok(self.clone_with_focus(xdm, 0))
    }
    pub fn clone_with_focus(&self, sequence: Xdm, position: usize) -> Self {
        DynamicContext {
            focus: Some(Focus { sequence, position }),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        }
    }
    pub fn clone_with_variable(&self, qname: QName, value: Xdm) -> Self {
        let mut ret = DynamicContext {
            focus: self.focus.clone(),
            static_context: Rc::clone(&self.static_context),
            variables: self.variables.clone(),
        };
        ret.set_variable(qname, value);
        ret
    }
    pub fn varref(&self, qname: &QName) -> Option<Xdm> {
        self.variables.get(qname).cloned()
    }
    pub fn set_variable(&mut self, qname: QName, value: Xdm) {
        self.variables.insert(qname, value);
    }
}
pub struct CompiledExpr(Box<dyn for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm>>);

impl CompiledExpr {
    pub fn new(
        closure: impl 'static + for<'context> Fn(&'context DynamicContext) -> XdmResult<Xdm>,
    ) -> Self {
        CompiledExpr(Box::new(closure))
    }
    pub fn execute<'context>(&self, context: &'context DynamicContext) -> XdmResult<Xdm> {
        self.0(context)
    }
}
