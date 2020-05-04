use rust_decimal::prelude::Zero;
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
    name: String,
    ns: Option<String>,
    prefix: Option<String>,
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
}
impl PartialEq for QName {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name) && self.ns.eq(&other.ns)
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
#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    RoXml(roxmltree::Node<'a, 'a>),
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
