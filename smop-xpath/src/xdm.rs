use nom::lib::std::hash::Hash;
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hasher;

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
