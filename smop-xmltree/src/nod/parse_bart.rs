use crate::nod::{Document, Idx, NodeData, NodeKind, QName};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use xmlparser::{ElementEnd, StrSpan, Token, Tokenizer};

#[derive(Debug)]
pub enum Error {
    Message(String),
    MisMatchedEndTag(String, String),
}
impl std::error::Error for Error {}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Message(m) => write!(f, "{}", m),
            Error::MisMatchedEndTag(start, end) => {
                write!(f, "mismatched start and end tags: {} vs {}", start, end)
            }
        }
    }
}

impl From<xmlparser::Error> for Error {
    fn from(xpe: xmlparser::Error) -> Self {
        Error::Message(format!("{:?}", xpe))
    }
}
impl Document {
    pub fn parse(text: &str) -> Result<Rc<Document>, Error> {
        Ok(Rc::new(parse(text)?))
    }
    fn push_node(&mut self, nd: NodeData) {
        self.nodes.push(nd)
    }
}

#[derive(Clone)]
struct ElemData<'input> {
    prefix: &'input str,
    local: &'input str,
    idx: Idx,
}
struct AttrData<'input> {
    prefix: &'input str,
    local: &'input str,
    value: &'input str,
}

struct ParserState<'input> {
    strings: HashMap<String, Idx>,
    qnames: HashMap<QName, Idx>,
    element_stack: Vec<ElemData<'input>>,
    attr_data: Vec<AttrData<'input>>,
}
impl ParserState<'_> {
    pub(crate) fn qname_idx(
        &mut self,
        doc: &mut Document,
        name: &str,
        ns: Option<&str>,
        prefix: Option<&str>,
    ) -> Idx {
        let qname = QName::new(
            name.to_string(),
            ns.map(String::from),
            prefix.map(String::from),
        );
        let idx = self.qnames.get(&qname);
        if idx.is_some() {
            *idx.unwrap()
        } else {
            let idx_usize = doc.qnames.len();
            let idx = Idx::new(idx_usize as u32);
            doc.qnames.push(qname.clone());
            self.qnames.insert(qname, idx);
            idx
        }
    }
}
fn parse(text: &str) -> Result<Document, Error> {
    let mut doc = Document {
        strings: vec![],
        qnames: vec![],
        nodes: vec![],
    };
    let root_data = NodeData {
        parent: None,
        prev_sibling: None,
        last_child: None,
        next_subtree: None,
        kind: NodeKind::Document,
    };
    doc.push_node(root_data);

    let mut state = ParserState {
        strings: Default::default(),
        qnames: Default::default(),
        element_stack: Vec::new(),
        attr_data: vec![],
    };
    let tokenizer = Tokenizer::from(text);
    for tok in tokenizer {
        let tok = tok?;
        println!("{:?}", &tok);

        match tok {
            Token::Declaration { .. } => {}
            Token::ProcessingInstruction { .. } => {}
            Token::Comment { .. } => {}
            Token::DtdStart { .. } => {}
            Token::EmptyDtd { .. } => {}
            Token::EntityDeclaration { .. } => {}
            Token::DtdEnd { .. } => {}
            Token::ElementStart { prefix, local, .. } => {
                state.attr_data = vec![];
                let idx: Idx = doc.nodes.len().into();
                let parent = state.element_stack.last().map_or(Idx::new(0), |e| e.idx);
                let node_data = NodeData {
                    parent: Some(parent),
                    prev_sibling: None,
                    last_child: None,
                    next_subtree: None,
                    kind: NodeKind::Element {
                        qname: Idx::new(0), // temporary, to be fixed on close of start tag
                    },
                };
                doc.nodes.push(node_data);
                state.element_stack.push(ElemData {
                    prefix: prefix.as_str(),
                    local: local.as_str(),
                    idx,
                });
            }
            Token::Attribute {
                prefix,
                local,
                value,
                ..
            } => {
                state.attr_data.push(AttrData {
                    prefix: prefix.as_str(),
                    local: local.as_str(),
                    value: value.as_str(),
                });
            }
            Token::ElementEnd { end, .. } => {
                //fn handle_close(e: &ElemData) {}
                match end {
                    ElementEnd::Open => {
                        let elem_data = state.element_stack.last().unwrap().clone();
                        handle_attributes(&elem_data, &mut state, &mut doc);
                    }
                    ElementEnd::Close(prefix, local) => {
                        let prefix = prefix.as_str();
                        let local = local.as_str();
                        let elem_data = state.element_stack.pop().unwrap();
                        if elem_data.prefix != prefix || elem_data.local != local {
                            let start = if elem_data.prefix.is_empty() || elem_data.local.is_empty()
                            {
                                format!("{}{}", elem_data.prefix, elem_data.local)
                            } else {
                                format!("{}:{}", elem_data.prefix, elem_data.local)
                            };
                            let end = if prefix.is_empty() || local.is_empty() {
                                format!("{}{}", prefix, local)
                            } else {
                                format!("{}:{}", prefix, local)
                            };
                            return Err(Error::MisMatchedEndTag(start, end));
                        }
                        handle_close(&elem_data, &mut state, &mut doc);
                    }
                    ElementEnd::Empty => {
                        let elem_data = state.element_stack.pop().unwrap();
                        handle_attributes(&elem_data, &mut state, &mut doc);
                        handle_close(&elem_data, &mut state, &mut doc);
                    }
                }
            }
            Token::Text { .. } => {}
            Token::Cdata { .. } => {}
        }
    }

    Ok(doc)
}

fn handle_attributes(e: &ElemData, state: &mut ParserState, doc: &mut Document) {
    // create namespaces
    // FIXME
    // resolve namespace for element name
    // FIXME
    // set element name
    let my_idx = e.idx.get_usize();
    let qname_idx = state.qname_idx(doc, e.local, None, None); // FIXME
    let node_data = doc.nodes.get(my_idx).unwrap();
    let node_data = NodeData {
        parent: node_data.parent,
        prev_sibling: None,
        last_child: None,
        next_subtree: None,
        kind: NodeKind::Element { qname: qname_idx },
    };
    doc.nodes[my_idx] = node_data;
    // resolve namespaces for attributes
    // create attributes
}

fn handle_close(e: &ElemData, state: &mut ParserState, doc: &mut Document) {
    let my_idx = e.idx.get_usize();
    let last_idx = doc.nodes.len() - 1;

    if last_idx > my_idx {
        let node_data = doc.nodes.get(my_idx).unwrap();
        let node_data = NodeData {
            parent: node_data.parent,
            prev_sibling: None,
            last_child: None,
            next_subtree: None,
            kind: node_data.kind.clone(),
        };
        doc.nodes[my_idx] = node_data;
    }
}
