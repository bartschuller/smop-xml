use rust_decimal::Decimal;
use std::collections::HashMap;

pub trait XdmNode {}

enum XdmAtomic {
    String(String),
    Boolean(bool),
    Decimal(Decimal),
    Double(f64),
}
enum XdmItem {
    Atomic(XdmAtomic),
    Node(Box<dyn XdmNode>),
    Array(Vec<XdmSequence>),
    Map(HashMap<XdmAtomic, XdmSequence>),
}

pub struct XdmSequence(Vec<XdmItem>);

impl XdmSequence {}
