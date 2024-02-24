use crate::ast::Expr;
use crate::functions::{Function, FunctionKey};
use crate::parser::parse;
use crate::runtime::DynamicContext;
use crate::types::{Item, Occurrence, SchemaType, SequenceType, TypeTree, Variety};
use crate::xdm::{XdmError, XdmResult};
use im::HashMap;
use regex::Regex;
use smop_xmltree::nod::QName;
use smop_xmltree::option_ext::OptionExt;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

pub struct Context {
    pub static_context: Rc<StaticContext>,
    pub dynamic_context: DynamicContext,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        let sc: Rc<StaticContext> = Rc::new(Default::default());
        let dc = sc.new_dynamic_context();
        Context {
            static_context: sc,
            dynamic_context: dc,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ExpandedName {
    ns: String,
    name: String,
}
impl ExpandedName {
    pub fn new(ns: &str, name: &str) -> Self {
        Self {
            ns: ns.to_string(),
            name: name.to_string(),
        }
    }
}
impl Display for ExpandedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.ns.is_empty() {
            write!(f, "Q{{{}}}{}", self.ns, self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}
impl From<&QName> for ExpandedName {
    fn from(qname: &QName) -> Self {
        ExpandedName {
            ns: qname.ns.as_str().unwrap_or("").to_string(),
            name: qname.name.clone(),
        }
    }
}
impl From<(&str, &str)> for ExpandedName {
    fn from(v: (&str, &str)) -> Self {
        ExpandedName {
            ns: v.0.to_string(),
            name: v.1.to_string(),
        }
    }
}
impl PartialEq<ExpandedName> for QName {
    fn eq(&self, other: &ExpandedName) -> bool {
        self.ns.as_str().unwrap_or("") == other.ns.as_str() && self.name == other.name
    }
}
impl PartialEq<QName> for ExpandedName {
    fn eq(&self, other: &QName) -> bool {
        self.ns.as_str() == other.ns.as_str().unwrap_or("") && self.name == other.name
    }
}

#[derive(Clone)]
pub struct StaticContext {
    namespaces: HashMap<String, String>,
    prefixes: HashMap<String, String>,
    schema_types: HashMap<ExpandedName, Rc<SchemaType>>,
    functions: HashMap<FunctionKey, Function>,
    default_function_namespace: Option<String>,
    default_element_namespace: Option<String>,
    variable_types: HashMap<ExpandedName, SequenceType>,
    pub context_item_type: SequenceType,
}

impl StaticContext {
    pub fn parse(&self, input: &str) -> XdmResult<Expr<()>> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^err:(\w+) (.*)$").unwrap();
        }
        #[allow(deprecated)]
        parse(self, input).map_err(|e| match RE.captures(e.description()) {
            Some(caps) => {
                XdmError::xqtm(caps.get(1).unwrap().as_str(), caps.get(2).unwrap().as_str())
            }
            None => XdmError::xqtm("XPST0003", e.to_string()),
        })
    }
    pub fn add_prefix_ns(&mut self, prefix: &str, ns: &str) {
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
        let k: ExpandedName = (t.ns.as_str().unwrap_or(""), t.name.as_str().unwrap()).into();
        self.schema_types.insert(k, t);
    }
    pub(crate) fn schema_type<E: Into<ExpandedName>>(&self, ename: E) -> XdmResult<Rc<SchemaType>> {
        let ename = ename.into();
        self.schema_types.get(&ename).cloned().ok_or(XdmError::xqtm(
            "XPST0008",
            format!("no schema definition for {} found", ename),
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
        self.functions.get(&key).or_else(|| {
            if ns.as_str() == Some("http://www.w3.org/2005/xpath-functions")
                && qname.name.as_str() == "concat"
                && arity > 2
            {
                let key = FunctionKey {
                    name: qname.name.clone(),
                    ns: ns?.clone(),
                    arity: 2,
                };
                self.functions.get(&key)
            } else {
                None
            }
        })
    }

    pub fn new_dynamic_context(self: &Rc<Self>) -> DynamicContext {
        DynamicContext {
            focus: None,
            static_context: Rc::clone(self),
            variables: HashMap::new(),
        }
    }

    pub fn variable_type<E: Into<ExpandedName>>(&self, ename: E) -> Option<SequenceType> {
        let ename = ename.into();
        self.variable_types.get(&ename).cloned()
    }

    pub fn set_variable_type<E: Into<ExpandedName>>(&mut self, ename: E, st: SequenceType) {
        let ename = ename.into();
        self.variable_types.insert(ename, st);
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
fn add_complex_type(sc: &mut StaticContext, qname: &str, base: Option<&str>) {
    let qname = QName::wellknown(qname);
    let base = base.map(|base| sc.schema_type(&QName::wellknown(base)).unwrap());
    let type_ = SchemaType {
        name: Some(qname.name.clone()),
        ns: qname.ns.as_ref().cloned(),
        prefix: qname.prefix.as_ref().cloned(),
        base_type: base,
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
            variable_types: HashMap::new(),
            context_item_type: SequenceType::Item(Item::Item, Occurrence::ZeroOrMore),
        };

        sc.add_prefix_ns("xs", "http://www.w3.org/2001/XMLSchema");
        sc.add_prefix_ns("fn", "http://www.w3.org/2005/xpath-functions");
        sc.add_prefix_ns("array", "http://www.w3.org/2005/xpath-functions/array");
        add_complex_type(&mut sc, "xs:anyType", None);
        add_complex_type(&mut sc, "xs:untyped", Some("xs:anyType"));
        add_simple_type(&mut sc, "xs:anySimpleType", "xs:anyType");
        add_simple_type(&mut sc, "xs:error", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:anyAtomicType", "xs:anySimpleType");
        add_simple_type(&mut sc, "xs:untypedAtomic", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:string", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:boolean", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:decimal", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:double", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:float", "xs:anyAtomicType");
        add_simple_type(&mut sc, "xs:integer", "xs:decimal");
        crate::xpath_functions_31::register(&mut sc);
        sc
    }
}

impl Debug for StaticContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[StaticContext]")
    }
}
