use crate::ast::Expr;
use crate::parser::parse;
use crate::types::{SchemaType, SimpleType, TypeTree, Variety};
use crate::xdm::{QName, XdmError, XdmResult};
use std::collections::HashMap;
use std::rc::Rc;

pub struct StaticContext {
    namespaces: HashMap<String, String>,
    schema_types: HashMap<QName, Rc<SchemaType>>,
}

impl<'i> StaticContext {
    pub fn parse(&self, input: &str) -> XdmResult<Expr> {
        parse(self, input).map_err(|e| XdmError::xqtm("XPST0003", &e.to_string()))
    }
    fn add_prefix_ns(&mut self, prefix: &str, ns: &str) {
        self.namespaces.insert(prefix.to_string(), ns.to_string());
    }
    pub fn prefix_defined(&self, prefix: &str) -> bool {
        self.namespaces.contains_key(prefix)
    }
    pub fn namespace(&self, prefix: &str) -> Option<String> {
        self.namespaces.get(prefix).cloned()
    }
    pub(crate) fn qname<S: AsRef<str>>(&self, prefix: S, local_name: S) -> Option<QName> {
        self.namespaces.get(prefix.as_ref()).map(|ns| {
            QName::new(
                local_name.as_ref().to_string(),
                Some(ns.to_string()),
                Some(prefix.as_ref().to_string()),
            )
        })
    }
    fn add_schema_type(&mut self, t: Rc<SchemaType>) {
        self.schema_types
            .insert(t.qname.as_ref().unwrap().clone(), t);
    }
    pub(crate) fn schema_type(&self, qname: &QName) -> XdmResult<Rc<SchemaType>> {
        self.schema_types.get(qname).cloned().ok_or(XdmError::xqtm(
            "XPST0008",
            &format!("no schema definition for {} found", qname),
        ))
    }
}

fn add_simple_type(sc: &mut StaticContext, qname: &str, base: &str) {
    let qname = QName::wellknown(qname);
    let base = sc.schema_type(&QName::wellknown(base)).unwrap();
    let type_ = SchemaType {
        qname: Some(qname.clone()),
        tree: TypeTree::Simple(Rc::new(SimpleType {
            name: Some(qname.name.clone()),
            ns: qname.ns.as_ref().cloned(),
            base_type: Some(base),
            variety: Variety::Atomic,
        })),
    };
    sc.add_schema_type(Rc::new(type_));
}
fn add_complex_type(sc: &mut StaticContext, qname: &str) {
    let qname = QName::wellknown(qname);
    let type_ = SchemaType {
        qname: Some(qname),
        tree: TypeTree::Complex,
    };
    sc.add_schema_type(Rc::new(type_));
}
impl Default for StaticContext {
    fn default() -> Self {
        let mut sc = StaticContext {
            namespaces: HashMap::new(),
            schema_types: HashMap::new(),
        };

        sc.add_prefix_ns("xs", "http://www.w3.org/2001/XMLSchema");
        add_complex_type(&mut sc, "xs:anyType");
        add_simple_type(&mut sc, "xs:anySimpleType", "xs:anyType");
        add_simple_type(&mut sc, "xs:error", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:anyAtomicType", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:string", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:boolean", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:decimal", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:double", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:integer", "xs:decimal");
        sc
    }
}
