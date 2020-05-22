use crate::ast::Expr;
use crate::functions::{Function, FunctionKey};
use crate::parser::parse;
use crate::runtime::DynamicContext;
use crate::types::{SchemaType, TypeTree, Variety};
use crate::xdm::{QName, XdmError, XdmResult};
use std::collections::HashMap;
use std::rc::Rc;

pub struct StaticContext {
    namespaces: HashMap<String, String>,
    prefixes: HashMap<String, String>,
    schema_types: HashMap<QName, Rc<SchemaType>>,
    functions: HashMap<FunctionKey, Function>,
    default_function_namespace: Option<String>,
    default_element_namespace: Option<String>,
}

impl StaticContext {
    pub fn parse(&self, input: &str) -> XdmResult<Expr> {
        parse(self, input).map_err(|e| XdmError::xqtm("XPST0003", &e.to_string()))
    }
    fn add_prefix_ns(&mut self, prefix: &str, ns: &str) {
        self.namespaces.insert(prefix.to_string(), ns.to_string());
        self.prefixes.insert(ns.to_string(), prefix.to_string());
    }
    pub fn prefix_defined(&self, prefix: &str) -> bool {
        self.namespaces.contains_key(prefix)
    }
    pub fn namespace(&self, prefix: &str) -> Option<String> {
        self.namespaces.get(prefix).cloned()
    }
    pub fn prefix(&self, ns: &str) -> Option<String> {
        self.prefixes.get(ns).cloned()
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
    pub(crate) fn qname_for_element(&self, qname: &mut QName) {
        if qname.ns.is_none() {
            qname.ns = self.default_element_namespace.clone();
        }
    }
    fn add_schema_type(&mut self, t: Rc<SchemaType>) {
        self.schema_types.insert(
            QName::new(
                t.name.as_ref().unwrap().clone(),
                t.ns.clone(),
                self.prefix(t.ns.as_ref().unwrap()),
            ),
            t,
        );
    }
    pub(crate) fn schema_type(&self, qname: &QName) -> XdmResult<Rc<SchemaType>> {
        self.schema_types.get(qname).cloned().ok_or(XdmError::xqtm(
            "XPST0008",
            &format!("no schema definition for {} found", qname),
        ))
    }
    pub(crate) fn add_function(&mut self, qname: QName, f: Function) {
        self.functions.insert(
            FunctionKey {
                name: qname.name,
                ns: qname.ns.unwrap(),
                arity: f.args.len(),
            },
            f,
        );
    }
    pub(crate) fn function(&self, qname: &QName, arity: usize) -> Option<&Function> {
        let ns = qname.ns.as_ref().or(if qname.prefix.is_none() {
            self.default_function_namespace.as_ref()
        } else {
            None
        });
        let key = FunctionKey {
            name: qname.name.clone(),
            ns: ns?.clone(),
            arity,
        };
        self.functions.get(&key)
    }

    pub fn new_dynamic_context<'a, 'input>(self: &Rc<Self>) -> DynamicContext<'a, 'input> {
        DynamicContext {
            focus: None,
            static_context: self.clone(),
        }
    }
}

fn add_simple_type(sc: &mut StaticContext, qname: &str, base: &str) {
    let qname = QName::wellknown(qname);
    let base = sc.schema_type(&QName::wellknown(base)).unwrap();
    let type_ = SchemaType {
        name: Some(qname.name.clone()),
        ns: qname.ns.as_ref().cloned(),
        prefix: qname.prefix.as_ref().cloned(),
        base_type: Some(base),
        tree: TypeTree::Simple(Variety::Atomic),
    };
    sc.add_schema_type(Rc::new(type_));
}
fn add_complex_type(sc: &mut StaticContext, qname: &str) {
    let qname = QName::wellknown(qname);
    let type_ = SchemaType {
        name: Some(qname.name.clone()),
        ns: qname.ns.as_ref().cloned(),
        prefix: qname.prefix.as_ref().cloned(),
        base_type: None,
        tree: TypeTree::Complex,
    };
    sc.add_schema_type(Rc::new(type_));
}
impl Default for StaticContext {
    fn default() -> Self {
        let mut sc = StaticContext {
            namespaces: HashMap::new(),
            prefixes: HashMap::new(),
            schema_types: HashMap::new(),
            functions: HashMap::new(),
            default_function_namespace: Some("http://www.w3.org/2005/xpath-functions".to_string()),
            default_element_namespace: None,
        };

        sc.add_prefix_ns("xs", "http://www.w3.org/2001/XMLSchema");
        sc.add_prefix_ns("fn", "http://www.w3.org/2005/xpath-functions");
        add_complex_type(&mut sc, "xs:anyType");
        add_simple_type(&mut sc, "xs:anySimpleType", "xs:anyType");
        add_simple_type(&mut sc, "xs:error", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:anyAtomicType", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:string", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:boolean", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:decimal", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:double", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:integer", "xs:decimal");
        crate::xpath_functions_31::register(&mut sc);
        sc
    }
}
