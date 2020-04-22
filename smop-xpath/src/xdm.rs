use rust_decimal::Decimal;
use std::collections::HashMap;

trait XdmNode {}

enum XdmAtomic {
    String(String),
    Boolean(bool),
    Decimal(Decimal),
    Double(f64),
}
enum XdmItem {
    XdmAtomic(XdmAtomic),
    XdmNode(Box<dyn XdmNode>),
    XdmArray(Vec<XdmSequence>),
    XdmMap(HashMap<XdmAtomic, XdmSequence>),
}

pub struct XdmSequence(Vec<XdmItem>);

impl XdmSequence {}
