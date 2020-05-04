use crate::xdm::QName;

#[derive(Debug, PartialEq)]
pub enum SequenceType {
    EmptySequence,
    Item(Item, Occurrence),
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Item,
    AtomicOrUnion(QName),
}

#[derive(Debug, PartialEq)]
pub enum Occurrence {
    One,
    Optional,
    ZeroOrMore,
    OneOrMore,
}

// reference: https://www.w3.org/TR/xmlschema-2/
// and https://www.w3.org/TR/xmlschema-1/#Simple_Type_Definition_details

pub enum SchemaType<'a> {
    Simple(SimpleType<'a>),
    Complex,
}

pub enum Variety<'a> {
    Atomic,
    List(&'a SimpleType<'a>),
    Union(Vec<&'a SimpleType<'a>>),
}

pub struct SimpleType<'a> {
    name: Option<String>,
    ns: Option<String>,
    base_type: Option<&'a SimpleType<'a>>,
    // facets
    // fundamental facets
    variety: Variety<'a>,
}

const UR: SimpleType = SimpleType {
    name: None,
    ns: None,
    base_type: None,
    variety: Variety::Atomic,
};

impl SimpleType<'_> {}
