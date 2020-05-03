// reference: https://www.w3.org/TR/xmlschema-2/
// and https://www.w3.org/TR/xmlschema-1/#Simple_Type_Definition_details

const xs: &str = "http://www.w3.org/2001/XMLSchema";

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

const ur: SimpleType = SimpleType {
    name: None,
    ns: None,
    base_type: None,
    variety: Variety::Atomic,
};

impl SimpleType<'_> {}
