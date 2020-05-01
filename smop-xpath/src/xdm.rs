use rust_decimal::prelude::Zero;
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::result::Result;

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
