use num_traits::cast::FromPrimitive;
use owning_ref::{BoxRef, OwningHandle};
use rust_decimal::prelude::{ToPrimitive, Zero};
use rust_decimal::Decimal;
use std::cell::{Ref, RefCell};
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

pub enum Node<'a> {
    RoXml(OwningHandle<Box<roxmltree::Document<'a>>, Box<roxmltree::Node<'a, 'a>>>),
}
impl Debug for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}
impl Clone for Node<'_> {
    fn clone(&self) -> Self {
        unimplemented!()
    }
}
impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Xdm<'a> {
    String(String),
    Boolean(bool),
    Decimal(Decimal),
    Integer(i64),
    Double(f64),
    Node(Node<'a>),
    Array(Vec<Xdm<'a>>),
    Map(HashMap<Xdm<'a>, Xdm<'a>>),
    Sequence(Vec<Xdm<'a>>),
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
    pub fn xqtm(code: &str, msg: &str) -> XdmError {
        XdmError {
            code: code.to_string(),
            message: msg.to_string(),
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
impl Xdm<'_> {
    pub fn boolean(&self) -> XdmResult<bool> {
        match self {
            Xdm::String(s) => Ok(s.len() > 0),
            Xdm::Boolean(b) => Ok(*b),
            Xdm::Decimal(d) => Ok(!d.is_zero()),
            Xdm::Integer(i) => Ok(*i != 0_i64),
            Xdm::Double(d) => Ok(!(d.is_nan() || d.is_zero())),
            Xdm::Node(_) => Ok(true),
            Xdm::Array(_) => Err(XdmError::xqtm("FORG0006", "boolean value of array")),
            Xdm::Map(_) => Err(XdmError::xqtm("FORG0006", "boolean value of map")),
            Xdm::Sequence(v) => {
                if v.is_empty() {
                    Ok(false)
                } else {
                    match v.first().unwrap() {
                        Xdm::Node(_) => Ok(true),
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
        match self {
            Xdm::Decimal(d) => Ok(d.to_i64().ok_or(XdmError::xqtm(
                "FOCA0003",
                "Input value too large for integer",
            ))?),
            Xdm::Integer(i) => Ok(*i),
            Xdm::Double(d) => Ok(*d as i64),
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
            _ => todo!("finish decimal conversion"),
        }
    }
}

impl Hash for Xdm<'_> {
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

impl Eq for Xdm<'_> {}

#[cfg(test)]
mod tests {
    use crate::xdm::QName;

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
