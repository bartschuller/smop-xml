use crate::roxml::AxisIter;
use crate::types::{Item, Occurrence, SequenceType};
use crate::StaticContext;
use num_traits::cast::FromPrimitive;
use roxmltree::{Attribute, ExpandedName, Node, NodeType};
use rust_decimal::prelude::{ToPrimitive, Zero};
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::result::Result;

// https://www.w3.org/TR/xpath-datamodel-31/#qnames-and-notations
#[derive(Debug, Clone)]
pub struct QName {
    pub name: String,
    pub ns: Option<String>,
    pub prefix: Option<String>,
}

impl QName {
    /// Note that a prefix is only allowed when a namespace is also provided. The following panics:
    /// ```should_panic
    /// # use xpath::xdm::QName;
    /// let wrong = QName::new("foo".to_string(), None, Some("wrong".to_string()));
    /// ```
    pub fn new(name: String, ns: Option<String>, prefix: Option<String>) -> Self {
        assert!(!(prefix.is_some() && ns.is_none()));
        QName { name, ns, prefix }
    }
    pub fn wellknown(s: &str) -> Self {
        let colon = s.find(':').unwrap();
        let prefix = &s[..colon];
        let name = &s[colon + 1..];
        let ns = match prefix {
            "xs" => "http://www.w3.org/2001/XMLSchema",
            &_ => panic!("not so well known prefix: {}", prefix),
        };
        QName {
            name: name.to_string(),
            ns: Some(ns.to_string()),
            prefix: Some(prefix.to_string()),
        }
    }
}
impl PartialEq for QName {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name) && self.ns.eq(&other.ns)
    }
}
impl Eq for QName {}
impl Hash for QName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ns.hash(state);
        self.name.hash(state)
    }
}
impl Display for QName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.ns.is_some() {
            if self.prefix.is_some() {
                write!(f, "{}:{}", self.prefix.as_ref().unwrap(), self.name)
            } else {
                write!(f, "Q{{{}}}{}", self.ns.as_ref().unwrap(), self.name)
            }
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl<'input> From<&'input QName> for ExpandedName<'input> {
    fn from(qname: &'input QName) -> Self {
        if let Some(ref ns) = qname.ns {
            (ns.as_str(), qname.name.as_str()).into()
        } else {
            qname.name.as_str().into()
        }
    }
}
pub enum NodeSeq<'a, 'input: 'a> {
    RoXml(Node<'a, 'input>),
    RoXmlIter(AxisIter<'a, 'input>),
    RoXmlAttr(&'a Attribute<'input>),
}
impl<'a, 'input: 'a> NodeSeq<'a, 'input> {}
impl Debug for NodeSeq<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NodeSeq::RoXml(n) => write!(f, "{:?}", n),
            NodeSeq::RoXmlIter(_) => write!(f, "node iterator"),
            NodeSeq::RoXmlAttr(a) => write!(f, "{:?}", a),
        }
    }
}
impl Clone for NodeSeq<'_, '_> {
    fn clone(&self) -> Self {
        match self {
            NodeSeq::RoXml(ro) => NodeSeq::RoXml(ro.clone()),
            NodeSeq::RoXmlIter(iter) => NodeSeq::RoXmlIter(iter.clone()),
            NodeSeq::RoXmlAttr(a) => NodeSeq::RoXmlAttr(a.clone()),
        }
    }
}
impl PartialEq for NodeSeq<'_, '_> {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}
impl Display for NodeSeq<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn ro_string_value(node: &roxmltree::Node, f: &mut Formatter<'_>) -> fmt::Result {
            match node.node_type() {
                NodeType::Root | NodeType::Element => {
                    for ref c in node.children() {
                        ro_string_value(c, f)?;
                    }
                    Ok(())
                }
                NodeType::PI => Ok(()),
                NodeType::Comment => Ok(()),
                NodeType::Text => write!(f, "{}", node.text().unwrap()),
            }
        }

        match self {
            NodeSeq::RoXml(n) => ro_string_value(n, f),
            NodeSeq::RoXmlIter(iter) => {
                for n in iter.clone() {
                    ro_string_value(&n, f)?;
                }
                Ok(())
            }
            NodeSeq::RoXmlAttr(a) => write!(f, "{}", a.value()),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Xdm<'a, 'input> {
    String(String),
    Boolean(bool),
    Decimal(Decimal),
    Integer(i64),
    Double(f64),
    NodeSeq(NodeSeq<'a, 'input>),
    Array(Vec<Xdm<'a, 'input>>),
    Map(HashMap<Xdm<'a, 'input>, Xdm<'a, 'input>>),
    Sequence(Vec<Xdm<'a, 'input>>),
}

pub type XdmResult<T> = Result<T, XdmError>;

#[derive(Debug, Clone, PartialEq)]
pub struct XdmError {
    // FIXME should be QName
    pub code: String,
    pub message: String,
}
impl XdmError {
    pub fn xqt(code: &str) -> XdmError {
        XdmError {
            code: code.to_string(),
            message: "".to_string(),
        }
    }
    pub fn xqtm<S1: Into<String>, S2: Into<String>>(code: S1, msg: S2) -> XdmError {
        XdmError {
            code: code.into(),
            message: msg.into(),
        }
    }
}
impl Error for XdmError {}

impl From<roxmltree::Error> for XdmError {
    fn from(re: roxmltree::Error) -> Self {
        XdmError::xqtm("err:FODC0006", re.to_string().as_str())
    }
}
impl Display for XdmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.code, self.message)
    }
}
impl Xdm<'_, '_> {
    pub fn flatten(v: Vec<Self>) -> Self {
        let mut res: Vec<Self> = Vec::new();

        fn add<'a, 'input>(dest: &mut Vec<Xdm<'a, 'input>>, v: Vec<Xdm<'a, 'input>>) {
            for x in v {
                match x {
                    Xdm::Sequence(inner) => add(dest, inner),
                    _ => dest.push(x),
                }
            }
        }

        add(&mut res, v);
        if res.len() == 1 {
            res.remove(0)
        } else {
            Xdm::Sequence(res)
        }
    }
    pub fn atomize(self) -> XdmResult<Self> {
        match self {
            Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_) => Ok(self),
            Xdm::NodeSeq(ns) => Ok(Xdm::String(ns.to_string())), // FIXME
            Xdm::Array(v) => {
                let res: XdmResult<Vec<_>> = v.into_iter().map(|x| x.atomize()).collect();
                Ok(Xdm::flatten(res?))
            }
            Xdm::Map(_) => Err(XdmError::xqtm(
                "err:FOTY0013",
                "The argument to fn:data() contains a map.",
            )),
            Xdm::Sequence(v) => {
                let res: XdmResult<Vec<_>> = v.into_iter().map(|x| x.atomize()).collect();
                Ok(Xdm::flatten(res?))
            }
        }
    }
    pub fn boolean(&self) -> XdmResult<bool> {
        match self {
            Xdm::String(s) => Ok(s.len() > 0),
            Xdm::Boolean(b) => Ok(*b),
            Xdm::Decimal(d) => Ok(!d.is_zero()),
            Xdm::Integer(i) => Ok(*i != 0_i64),
            Xdm::Double(d) => Ok(!(d.is_nan() || d.is_zero())),
            Xdm::NodeSeq(_) => Ok(true),
            Xdm::Array(_) => Err(XdmError::xqtm("FORG0006", "boolean value of array")),
            Xdm::Map(_) => Err(XdmError::xqtm("FORG0006", "boolean value of map")),
            Xdm::Sequence(v) => {
                if v.is_empty() {
                    Ok(false)
                } else {
                    match v.first().unwrap() {
                        Xdm::NodeSeq(_) => Ok(true),
                        _ => Err(XdmError::xqtm(
                            "FORG0006",
                            "boolean of sequence of non-node",
                        )),
                    }
                }
            }
        }
    }
    pub fn integer(&self) -> XdmResult<i64> {
        match self.clone().atomize()? {
            Xdm::Decimal(d) => Ok(d.to_i64().ok_or(XdmError::xqtm(
                "FOCA0003",
                "Input value too large for integer",
            ))?),
            Xdm::Integer(i) => Ok(i),
            Xdm::Double(d) => Ok(d as i64),
            Xdm::String(s) => s.parse::<i64>().map_err(|e| {
                XdmError::xqtm(
                    "err:FORG0001",
                    format!("Can't cast string '{}' to an integer", s),
                )
            }),
            _ => todo!("finish integer conversion"),
        }
    }
    pub fn decimal(&self) -> XdmResult<Decimal> {
        match self {
            Xdm::Decimal(d) => Ok(d.clone()),
            Xdm::Integer(i) => Ok(Decimal::new(*i, 0)),
            Xdm::Double(d) => {
                Ok(Decimal::from_f64(*d)
                    .ok_or(XdmError::xqtm("FOCA0002", "Invalid lexical value"))?)
            }
            _ => todo!("finish decimal conversion"),
        }
    }
    pub fn double(&self) -> XdmResult<f64> {
        match self {
            Xdm::Decimal(d) => Ok(d.to_f64().unwrap()),
            Xdm::Integer(i) => Ok(*i as f64),
            Xdm::Double(d) => Ok(*d),
            _ => todo!("finish double conversion"),
        }
    }
    pub fn string(&self) -> XdmResult<String> {
        match self {
            Xdm::String(s) => Ok(s.clone()),
            Xdm::Boolean(b) => Ok(if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            Xdm::Decimal(d) => Ok(d.to_string()),
            Xdm::Integer(i) => Ok(i.to_string()),
            Xdm::Double(d) => Ok(d.to_string()),
            Xdm::NodeSeq(n) => Ok(n.to_string()),
            Xdm::Array(_) => Err(XdmError::xqtm(
                "err:FOTY0014",
                "can't convert an array to a string",
            )),
            Xdm::Map(_) => Err(XdmError::xqtm(
                "err:FOTY0014",
                "can't convert a map to a string",
            )),
            Xdm::Sequence(v) => {
                if v.is_empty() {
                    Ok("".to_string())
                } else {
                    Err(XdmError::xqtm(
                        "XPTY0004",
                        "can't convert a sequence to a string",
                    ))
                }
            }
        }
    }
    pub fn dynamic_type(&self, ctx: &StaticContext) -> XdmResult<SequenceType> {
        fn simple(ctx: &StaticContext, xs: &str) -> XdmResult<SequenceType> {
            Ok(SequenceType::Item(
                Item::AtomicOrUnion(ctx.schema_type(&QName::wellknown(xs))?),
                Occurrence::One,
            ))
        }
        match self {
            Xdm::String(_) => simple(ctx, "xs:string"),
            Xdm::Boolean(_) => simple(ctx, "xs:boolean"),
            Xdm::Decimal(_) => simple(ctx, "xs:decimal"),
            Xdm::Integer(_) => simple(ctx, "xs:integer"),
            Xdm::Double(_) => simple(ctx, "xs:double"),
            Xdm::NodeSeq(_) => unimplemented!(),
            Xdm::Array(_) => unimplemented!(),
            Xdm::Map(_) => unimplemented!(),
            Xdm::Sequence(v) => {
                if v.is_empty() {
                    Ok(SequenceType::EmptySequence)
                } else {
                    unimplemented!()
                }
            }
        }
    }
}

impl Hash for Xdm<'_, '_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Xdm::String(s) => s.hash(state),
            Xdm::Boolean(b) => b.hash(state),
            Xdm::Decimal(d) => d.hash(state),
            Xdm::Integer(i) => i.hash(state),
            Xdm::Double(_) => panic!("attempt to use a double as a hash key"),
            _ => panic!("attempt to use non-atomic value as a hash key"),
        }
    }
}

impl Eq for Xdm<'_, '_> {}

impl<'a, 'input: 'a> IntoIterator for Xdm<'a, 'input> {
    type Item = Xdm<'a, 'input>;
    type IntoIter = Box<dyn Iterator<Item = Xdm<'a, 'input>> + 'a>;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            Xdm::String(_)
            | Xdm::Boolean(_)
            | Xdm::Decimal(_)
            | Xdm::Integer(_)
            | Xdm::Double(_) => Box::new(std::iter::once(self)),

            // Xdm::NodeSeq(_) => {}
            // Xdm::Array(_) => {}
            // Xdm::Map(_) => {}
            Xdm::Sequence(v) => Box::new(v.into_iter()),
            _ => todo!("IntoIterator for Xdm"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::xdm::{QName, XdmResult};
    use roxmltree::Document;

    #[test]
    fn qname1() {
        let qname1 = QName::new("local".to_string(), None, None);
        let qname2 = QName::new("local".to_string(), Some("".to_string()), None);
        let qname3 = QName::new(
            "local".to_string(),
            Some("http://example.com/".to_string()),
            None,
        );
        let qname4 = QName::new(
            "local".to_string(),
            Some("http://example.com/".to_string()),
            Some("ex".to_string()),
        );
        let qname5 = QName::new(
            "local".to_string(),
            Some("http://example.com/".to_string()),
            Some("ex2".to_string()),
        );
        let qname6 = QName::new("other".to_string(), None, None);
        assert_ne!(qname1, qname2);
        assert_ne!(qname2, qname3);
        assert_ne!(qname1, qname6);

        assert_eq!(qname3, qname4);
        assert_eq!(qname4, qname5);
        assert_eq!(qname5, qname3);

        assert_eq!(format!("{}", qname1), "local");
        assert_eq!(format!("{}", qname2), "Q{}local");
        assert_eq!(format!("{}", qname3), "Q{http://example.com/}local");
        assert_eq!(format!("{}", qname4), "ex:local");
    }
}
