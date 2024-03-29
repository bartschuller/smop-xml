use std::borrow::{Borrow, Cow};
use std::error;
use std::fmt;
use std::str;

use crate::nod::{
    Document, Idx, Namespaces, NodeData, NodeType, QName, ShortRange, NS_XMLNS_URI, NS_XML_URI, PI,
};
use std::collections::HashMap;
use xmlparser::{self, ElementEnd, Reference, StrSpan, Stream, TextPos};

/// A list of all possible errors.
#[derive(Debug)]
pub enum Error {
    /// The `xmlns:xml` attribute must have an <http://www.w3.org/XML/1998/namespace> URI.
    InvalidXmlPrefixUri(TextPos),

    /// Only the `xmlns:xml` attribute can have the <http://www.w3.org/XML/1998/namespace> URI.
    UnexpectedXmlUri(TextPos),

    /// The <http://www.w3.org/2000/xmlns/> URI must not be declared.
    UnexpectedXmlnsUri(TextPos),

    /// `xmlns` can't be used as an element prefix.
    InvalidElementNamePrefix(TextPos),

    /// A namespace was already defined on this element.
    DuplicatedNamespace(String, TextPos),

    /// An unknown namespace.
    ///
    /// Indicates that an element or an attribute has an unknown qualified name prefix.
    ///
    /// The first value is a prefix.
    UnknownNamespace(String, TextPos),

    /// Incorrect tree structure.
    #[allow(missing_docs)]
    UnexpectedCloseTag {
        expected: String,
        actual: String,
        pos: TextPos,
    },

    /// Entity value starts with a close tag.
    ///
    /// Example:
    /// ```xml
    /// <!DOCTYPE test [ <!ENTITY p '</p>'> ]>
    /// <root>&p;</root>
    /// ```
    UnexpectedEntityCloseTag(TextPos),

    /// A reference to an entity that was not defined in the DTD.
    UnknownEntityReference(String, TextPos),

    /// A malformed entity reference.
    ///
    /// A `&` character inside an attribute value or text indicates an entity reference.
    /// Otherwise, the document is not well-formed.
    MalformedEntityReference(TextPos),

    /// A possible entity reference loop.
    ///
    /// The current depth limit is 10. The max number of references per reference is 255.
    EntityReferenceLoop(TextPos),

    /// Attribute value cannot have a `<` character.
    InvalidAttributeValue(TextPos),

    /// An element has a duplicated attributes.
    ///
    /// This also includes namespaces resolving.
    /// So an element like this will lead to an error.
    /// ```xml
    /// <e xmlns:n1='http://www.w3.org' xmlns:n2='http://www.w3.org' n1:a='b1' n2:a='b2'/>
    /// ```
    DuplicatedAttribute(String, TextPos),

    /// The XML document must have at least one element.
    NoRootNode,

    /// The input string should be smaller than 4GiB.
    SizeLimit,

    /// Errors detected by the `xmlparser` crate.
    ParserError(xmlparser::Error),
}

impl Error {
    /// Returns the error position.
    #[inline]
    pub fn pos(&self) -> TextPos {
        match *self {
            Error::InvalidXmlPrefixUri(pos) => pos,
            Error::UnexpectedXmlUri(pos) => pos,
            Error::UnexpectedXmlnsUri(pos) => pos,
            Error::InvalidElementNamePrefix(pos) => pos,
            Error::DuplicatedNamespace(ref _name, pos) => pos,
            Error::UnknownNamespace(ref _name, pos) => pos,
            Error::UnexpectedCloseTag { pos, .. } => pos,
            Error::UnexpectedEntityCloseTag(pos) => pos,
            Error::UnknownEntityReference(ref _name, pos) => pos,
            Error::MalformedEntityReference(pos) => pos,
            Error::EntityReferenceLoop(pos) => pos,
            Error::InvalidAttributeValue(pos) => pos,
            Error::DuplicatedAttribute(ref _name, pos) => pos,
            Error::ParserError(ref err) => err.pos(),
            _ => TextPos::new(1, 1),
        }
    }
}

impl From<xmlparser::Error> for Error {
    #[inline]
    fn from(e: xmlparser::Error) -> Self {
        Error::ParserError(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidXmlPrefixUri(pos) => {
                write!(f, "'xml' namespace prefix mapped to wrong URI at {}", pos)
            }
            Error::UnexpectedXmlUri(pos) => write!(
                f,
                "the 'xml' namespace URI is used for not 'xml' prefix at {}",
                pos
            ),
            Error::UnexpectedXmlnsUri(pos) => write!(
                f,
                "the 'xmlns' URI is used at {}, but it must not be declared",
                pos
            ),
            Error::InvalidElementNamePrefix(pos) => write!(
                f,
                "the 'xmlns' prefix is used at {}, but it must not be",
                pos
            ),
            Error::DuplicatedNamespace(ref name, pos) => {
                write!(f, "namespace '{}' at {} is already defined", name, pos)
            }
            Error::UnknownNamespace(ref name, pos) => {
                write!(f, "an unknown namespace prefix '{}' at {}", name, pos)
            }
            Error::UnexpectedCloseTag {
                ref expected,
                ref actual,
                pos,
            } => write!(
                f,
                "expected '{}' tag, not '{}' at {}",
                expected, actual, pos
            ),
            Error::UnexpectedEntityCloseTag(pos) => write!(f, "unexpected close tag at {}", pos),
            Error::MalformedEntityReference(pos) => {
                write!(f, "malformed entity reference at {}", pos)
            }
            Error::UnknownEntityReference(ref name, pos) => {
                write!(f, "unknown entity reference '{}' at {}", name, pos)
            }
            Error::EntityReferenceLoop(pos) => {
                write!(f, "a possible entity reference loop is detected at {}", pos)
            }
            Error::InvalidAttributeValue(pos) => write!(f, "unescaped '<' found at {}", pos),
            Error::DuplicatedAttribute(ref name, pos) => {
                write!(f, "attribute '{}' at {} is already defined", name, pos)
            }
            Error::NoRootNode => write!(f, "the document does not have a root node"),
            Error::SizeLimit => write!(f, "the input string should be smaller than 4GiB"),
            Error::ParserError(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for Error {
    #[inline]
    fn description(&self) -> &str {
        "an XML parsing error"
    }
}

struct AttributeData<'input> {
    prefix: StrSpan<'input>,
    local: StrSpan<'input>,
    value: Cow<'input, str>,
    value_range: ShortRange,
}

impl Document {
    /// Parses the input XML string.
    ///
    /// We do not support `&[u8]` or `Reader` because the input must be an already allocated
    /// UTF-8 string.
    ///
    /// # Examples
    ///
    /// ```
    /// let doc = smop_xmltree::nod::Document::parse("<e/>").unwrap();
    /// assert_eq!(doc.root().descendants_or_self().count(), 2); // root node + `e` element node
    /// ```
    #[inline]
    pub fn parse(text: &str) -> Result<Rc<Document>, Error> {
        parse(text)
    }

    fn append(
        &mut self,
        parent_id: Idx,
        node_type: NodeType,
        range: ShortRange,
        pd_awaiting_subtree: &mut Vec<Idx>,
    ) -> Idx {
        let new_child_id = Idx::from(self.nodes.len());

        let appending_element = node_type.is_element();
        let appending_attribute = node_type.is_attribute();

        self.nodes.push(NodeData {
            parent: Some(parent_id),
            prev_sibling: None,
            next_subtree: None,
            last_child: None,
            node_type,
            range,
        });

        if appending_attribute {
            let previous_attribute = new_child_id.get_usize() - 1;
            if self.nodes[previous_attribute].node_type.is_attribute() {
                self.nodes[new_child_id.get_usize()].prev_sibling =
                    Some(Idx::from(previous_attribute));
                self.nodes[previous_attribute].next_subtree = Some(new_child_id);
            }
        } else {
            let last_child_id = self.nodes[parent_id.get_usize()].last_child;
            self.nodes[new_child_id.get_usize()].prev_sibling = last_child_id;
            self.nodes[parent_id.get_usize()].last_child = Some(new_child_id);

            pd_awaiting_subtree.iter().for_each(|id| {
                self.nodes[id.get_usize()].next_subtree = Some(new_child_id);
            });
            pd_awaiting_subtree.clear();

            if !appending_element {
                pd_awaiting_subtree.push(Idx::from(self.nodes.len() - 1));
            }
        }

        new_child_id
    }
}

struct Entity<'input> {
    name: &'input str,
    value: StrSpan<'input>,
}

struct ParserData<'input> {
    //attrs_start_idx: usize,
    ns_start_idx: usize,
    tmp_attrs: Vec<AttributeData<'input>>,
    awaiting_subtree: Vec<Idx>,
    entities: Vec<Entity<'input>>,
    buffer: TextBuffer,
    after_text: bool,
    qnames: HashMap<QName, Idx>,
    strings: HashMap<String, Idx>,
}

pub(crate) fn qname_idx(
    qnames_hash: &mut HashMap<QName, Idx>,
    doc_qnames: &mut Vec<QName>,
    name: &str,
    ns: Option<Cow<str>>,
    prefix: Option<&str>,
) -> Idx {
    let qname = QName::new(
        name.to_string(),
        ns.map(String::from),
        prefix.map(String::from),
    );
    let idx = qnames_hash.get(&qname);
    if idx.is_some() {
        *idx.unwrap()
    } else {
        let idx_usize = doc_qnames.len();
        let idx = Idx::new(idx_usize as u32);
        doc_qnames.push(qname.clone());
        qnames_hash.insert(qname, idx);
        idx
    }
}

pub(crate) fn string_idx(
    strings_hash: &mut HashMap<String, Idx>,
    doc_strings: &mut Vec<String>,
    value: &str,
) -> Idx {
    let idx = strings_hash.get(value);
    if idx.is_some() {
        *idx.unwrap()
    } else {
        let idx_usize = doc_strings.len();
        let idx = Idx::new(idx_usize as u32);
        doc_strings.push(value.to_string());
        strings_hash.insert(value.to_string(), idx);
        idx
    }
}

#[derive(Clone, Copy)]
struct TagNameSpan<'input> {
    prefix: StrSpan<'input>,
    name: StrSpan<'input>,
    span: StrSpan<'input>,
}

impl<'input> TagNameSpan<'input> {
    #[inline]
    fn new_null() -> Self {
        Self {
            prefix: StrSpan::from(""),
            name: StrSpan::from(""),
            span: StrSpan::from(""),
        }
    }

    #[inline]
    fn new(prefix: StrSpan<'input>, name: StrSpan<'input>, span: StrSpan<'input>) -> Self {
        Self { prefix, name, span }
    }
}

/// An entity loop detector.
///
/// Limits:
/// - Entities depth is 10.
/// - Maximum number of entity references per entity reference is 255.
///
/// Basically, if a text or an attribute has an entity reference and this reference
/// has more than 10 nested references - this is an error.
///
/// This is useful for simple loops like:
///
/// ```text
/// <!ENTITY a '&b;'>
/// <!ENTITY b '&a;'>
/// ```
///
/// And, if a text or an attribute has an entity reference and it references more
/// than 255 references - this is an error.
///
/// This is useful for cases like billion laughs attack, where depth can be pretty small,
/// but the number of references is exponentially increasing:
///
/// ```text
/// <!ENTITY lol "lol">
/// <!ENTITY lol1 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
/// <!ENTITY lol2 "&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;">
/// <!ENTITY lol3 "&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;">
/// <!ENTITY lol4 "&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;">
/// ```
#[derive(Default)]
struct LoopDetector {
    /// References depth.
    depth: u8,
    /// Number of references resolved by the root reference.
    references: u8,
}

impl LoopDetector {
    #[inline]
    fn inc_depth(&mut self, s: &Stream) -> Result<(), Error> {
        if self.depth < 10 {
            self.depth += 1;
            Ok(())
        } else {
            Err(Error::EntityReferenceLoop(s.gen_text_pos()))
        }
    }

    #[inline]
    fn dec_depth(&mut self) {
        if self.depth > 0 {
            self.depth -= 1;
        }

        // Reset references count after reaching zero depth.
        if self.depth == 0 {
            self.references = 0;
        }
    }

    #[inline]
    fn inc_references(&mut self, s: &Stream) -> Result<(), Error> {
        if self.depth == 0 {
            // Allow infinite amount of references at zero depth.
            Ok(())
        } else {
            if self.references == std::u8::MAX {
                return Err(Error::EntityReferenceLoop(s.gen_text_pos()));
            }

            self.references += 1;
            Ok(())
        }
    }
}

fn parse(text: &str) -> Result<Rc<Document>, Error> {
    if text.len() > std::u32::MAX as usize {
        return Err(Error::SizeLimit);
    }

    let mut pd = ParserData {
        ns_start_idx: 1,
        tmp_attrs: Vec::with_capacity(16),
        entities: Vec::new(),
        awaiting_subtree: Vec::new(),
        buffer: TextBuffer::new(),
        after_text: false,
        qnames: Default::default(),
        strings: Default::default(),
    };

    // Trying to guess rough nodes and attributes amount.
    let nodes_capacity = text.bytes().filter(|c| *c == b'<').count();
    let attributes_capacity = text.bytes().filter(|c| *c == b'=').count();

    // Init document.
    let mut doc = Document {
        strings: vec![],
        qnames: vec![],
        nodes: Vec::with_capacity(nodes_capacity + attributes_capacity),
        namespaces: Namespaces(Vec::new()),
    };

    // Add a root node.
    doc.nodes.push(NodeData {
        parent: None,
        prev_sibling: None,
        next_subtree: None,
        last_child: None,
        node_type: NodeType::Document,
        range: (0..text.len()).into(),
    });

    doc.namespaces
        .push_ns(Some("xml".to_string()), NS_XML_URI.to_string());

    let parser = xmlparser::Tokenizer::from(text);
    let parent_id = Idx::new(0);
    let mut tag_name = TagNameSpan::new_null();
    process_tokens(
        parser,
        parent_id,
        &mut LoopDetector::default(),
        &mut tag_name,
        &mut pd,
        &mut doc,
        text,
    )?;

    doc.nodes.shrink_to_fit();
    doc.namespaces.0.shrink_to_fit();

    let doc = Rc::new(doc);
    if !doc.root().children().any(|n| n.is_element()) {
        return Err(Error::NoRootNode);
    }

    Ok(doc)
}

fn process_tokens<'input>(
    parser: xmlparser::Tokenizer<'input>,
    mut parent_id: Idx,
    loop_detector: &mut LoopDetector,
    tag_name: &mut TagNameSpan<'input>,
    pd: &mut ParserData<'input>,
    doc: &mut Document,
    doc_text: &'input str,
) -> Result<(), Error> {
    for token in parser {
        let token = token?;
        match token {
            xmlparser::Token::ProcessingInstruction {
                target,
                content,
                span,
            } => {
                let target = string_idx(&mut pd.strings, &mut doc.strings, target.as_str());
                let value =
                    content.map(|v| string_idx(&mut pd.strings, &mut doc.strings, v.as_str()));
                let pi = NodeType::PI(PI { target, value });
                doc.append(parent_id, pi, span.range().into(), &mut pd.awaiting_subtree);
            }
            xmlparser::Token::Comment { text, span } => {
                let string_idx = string_idx(&mut pd.strings, &mut doc.strings, text.as_str());
                doc.append(
                    parent_id,
                    NodeType::Comment(string_idx),
                    span.range().into(),
                    &mut pd.awaiting_subtree,
                );
            }
            xmlparser::Token::Text { text } => {
                process_text(text, parent_id, loop_detector, pd, doc, doc_text)?;
            }
            xmlparser::Token::Cdata { text, span } => {
                let cow_str = Cow::Borrowed(text.as_str());
                append_text(
                    cow_str,
                    parent_id,
                    span.range().into(),
                    pd.after_text,
                    doc,
                    pd,
                );
                pd.after_text = true;
            }
            xmlparser::Token::ElementStart {
                prefix,
                local,
                span,
            } => {
                if prefix.as_str() == "xmlns" {
                    let pos = err_pos_from_span(doc_text, prefix);
                    return Err(Error::InvalidElementNamePrefix(pos));
                }

                *tag_name = TagNameSpan::new(prefix, local, span);
            }
            xmlparser::Token::Attribute {
                prefix,
                local,
                value,
                span: _,
            } => {
                process_attribute(prefix, local, value, loop_detector, pd, doc, doc_text)?;
            }
            xmlparser::Token::ElementEnd { end, span } => {
                process_element(*tag_name, end, span, &mut parent_id, pd, doc, doc_text)?;
            }
            xmlparser::Token::EntityDeclaration {
                name, definition, ..
            } => {
                if let xmlparser::EntityDefinition::EntityValue(value) = definition {
                    pd.entities.push(Entity {
                        name: name.as_str(),
                        value,
                    });
                }
            }
            _ => {}
        }

        match token {
            xmlparser::Token::ProcessingInstruction { .. }
            | xmlparser::Token::Comment { .. }
            | xmlparser::Token::ElementStart { .. }
            | xmlparser::Token::ElementEnd { .. } => {
                pd.after_text = false;
            }
            _ => {}
        }
    }

    Ok(())
}

fn process_attribute<'input>(
    prefix: StrSpan<'input>,
    local: StrSpan<'input>,
    value: StrSpan<'input>,
    loop_detector: &mut LoopDetector,
    pd: &mut ParserData<'input>,
    doc: &mut Document,
    text: &'input str,
) -> Result<(), Error> {
    let value_range = value.range().into();
    let value = normalize_attribute(text, value, &pd.entities, loop_detector, &mut pd.buffer)?;

    if prefix.as_str() == "xmlns" {
        // The xmlns namespace MUST NOT be declared as the default namespace.
        if NS_XMLNS_URI == value {
            let pos = err_pos_from_qname(text, prefix, local);
            return Err(Error::UnexpectedXmlnsUri(pos));
        }

        let is_xml_ns_uri = NS_XML_URI == value;

        // The prefix 'xml' is by definition bound to the namespace name
        // http://www.w3.org/XML/1998/namespace.
        // It MUST NOT be bound to any other namespace name.
        if local.as_str() == "xml" {
            if !is_xml_ns_uri {
                let pos = err_pos_from_span(text, prefix);
                return Err(Error::InvalidXmlPrefixUri(pos));
            }
        } else {
            // The xml namespace MUST NOT be bound to a non-xml prefix.
            if is_xml_ns_uri {
                let pos = err_pos_from_span(text, prefix);
                return Err(Error::UnexpectedXmlUri(pos));
            }
        }

        // Check for duplicated namespaces.
        if doc.namespaces.exists(pd.ns_start_idx, Some(local.as_str())) {
            let pos = err_pos_from_qname(text, prefix, local);
            return Err(Error::DuplicatedNamespace(local.as_str().to_string(), pos));
        }

        // Xml namespace should not be added to the namespaces.
        if !is_xml_ns_uri {
            doc.namespaces
                .push_ns(Some(local.as_str().to_string()), value.to_string());
        }
    } else if local.as_str() == "xmlns" {
        // The xml namespace MUST NOT be declared as the default namespace.
        if NS_XML_URI == value {
            let pos = err_pos_from_span(text, local);
            return Err(Error::UnexpectedXmlUri(pos));
        }

        // The xmlns namespace MUST NOT be declared as the default namespace.
        if NS_XMLNS_URI == value {
            let pos = err_pos_from_span(text, local);
            return Err(Error::UnexpectedXmlnsUri(pos));
        }

        doc.namespaces.push_ns(None, value.to_string());
    } else {
        pd.tmp_attrs.push(AttributeData {
            prefix,
            local,
            value,
            value_range,
        });
    }

    Ok(())
}

fn process_element<'input>(
    tag_name: TagNameSpan<'input>,
    end_token: xmlparser::ElementEnd<'input>,
    token_span: StrSpan<'input>,
    parent_id: &mut Idx,
    pd: &mut ParserData<'input>,
    doc: &mut Document,
    text: &'input str,
) -> Result<(), Error> {
    if tag_name.name.is_empty() {
        // May occur in XML like this:
        // <!DOCTYPE test [ <!ENTITY p '</p>'> ]>
        // <root>&p;</root>

        if let xmlparser::ElementEnd::Close(..) = end_token {
            return Err(Error::UnexpectedEntityCloseTag(err_pos_from_span(
                text, token_span,
            )));
        } else {
            unreachable!("should be already checked by the xmlparser");
        }
    }

    let namespaces = resolve_namespaces(pd.ns_start_idx, *parent_id, doc);
    pd.ns_start_idx = doc.namespaces.len();

    let new_element_id: Option<Idx> = match end_token {
        ElementEnd::Open | ElementEnd::Empty => {
            let tag_ns_uri = get_ns_by_prefix(doc, namespaces, tag_name.prefix, text)?;
            let prefix = if tag_name.prefix.as_str() == "" {
                None
            } else {
                Some(tag_name.prefix.as_str())
            };
            let qname_idx = qname_idx(
                &mut pd.qnames,
                &mut doc.qnames,
                tag_name.name.as_str(),
                tag_ns_uri,
                prefix,
            );
            let new_element_id = doc.append(
                *parent_id,
                NodeType::Element {
                    qname: qname_idx,
                    num_attributes: 0,
                    namespaces,
                },
                (tag_name.span.start()..token_span.end()).into(),
                &mut pd.awaiting_subtree,
            );
            let final_num_attributes =
                resolve_attributes(new_element_id, namespaces, doc, text, pd)?;
            if let NodeType::Element {
                qname,
                num_attributes: _,
                namespaces,
            } = doc.nodes[new_element_id.get_usize()].node_type
            {
                doc.nodes[new_element_id.get_usize()].node_type = NodeType::Element {
                    qname,
                    num_attributes: final_num_attributes,
                    namespaces,
                }
            }
            pd.tmp_attrs.clear();
            Some(new_element_id)
        }
        ElementEnd::Close(_, _) => None,
    };

    match end_token {
        xmlparser::ElementEnd::Empty => {
            pd.awaiting_subtree.push(new_element_id.unwrap());
        }
        xmlparser::ElementEnd::Close(prefix, local) => {
            let prefix = if prefix.as_str() == "" {
                None
            } else {
                Some(prefix.as_str())
            };
            let local = local.as_str();

            doc.nodes[parent_id.get_usize()].range.end = token_span.end() as u32;
            if let NodeType::Element { ref qname, .. } = doc.nodes[parent_id.get_usize()].node_type
            {
                let qname = &doc.qnames[qname.get_usize()];
                if prefix != qname.prefix.as_str() || local != qname.name {
                    return Err(Error::UnexpectedCloseTag {
                        expected: gen_qname_string(qname.prefix.as_str(), qname.name.as_str()),
                        actual: gen_qname_string(prefix, local),
                        pos: err_pos_from_span(text, token_span),
                    });
                }
            }
            pd.awaiting_subtree.push(*parent_id);

            if let Some(id) = doc.nodes[parent_id.get_usize()].parent {
                *parent_id = id;
            } else {
                unreachable!("should be already checked by the xmlparser");
            }
        }
        xmlparser::ElementEnd::Open => {
            *parent_id = new_element_id.unwrap();
        }
    }

    Ok(())
}

fn resolve_namespaces(start_idx: usize, parent_id: Idx, doc: &mut Document) -> ShortRange {
    if let NodeType::Element { ref namespaces, .. } = doc.nodes[parent_id.get_usize()].node_type {
        let parent_ns = *namespaces;
        if start_idx == doc.namespaces.len() {
            return parent_ns;
        }

        for i in parent_ns.to_urange() {
            if !doc
                .namespaces
                .exists(start_idx, doc.namespaces[i].name.as_str())
            {
                let v = doc.namespaces[i].clone();
                doc.namespaces.0.push(v);
            }
        }
    }

    (start_idx..doc.namespaces.len()).into()
}

// returns number of attributes
fn resolve_attributes(
    element_id: Idx,
    namespaces: ShortRange,
    doc: &mut Document,
    text: &str,
    pd: &mut ParserData,
) -> Result<u32, Error> {
    let start_idx = doc.nodes.len();
    let tmp_attrs = &mut pd.tmp_attrs;

    if tmp_attrs.is_empty() {
        return Ok(0);
    }

    for attr in tmp_attrs {
        let ns = if attr.prefix.as_str() == "xml" {
            // The prefix 'xml' is by definition bound to the namespace name
            // http://www.w3.org/XML/1998/namespace.
            Some(Cow::Borrowed(NS_XML_URI))
        } else if attr.prefix.is_empty() {
            // 'The namespace name for an unprefixed attribute name
            // always has no value.'
            None
        } else {
            get_ns_by_prefix(doc, namespaces, attr.prefix, text)?
        };

        let attr_name_idx = qname_idx(
            &mut pd.qnames,
            &mut doc.qnames,
            attr.local.as_str(),
            ns,
            if attr.prefix.is_empty() {
                None
            } else {
                Some(attr.prefix.as_str())
            },
        );
        // TODO Check for duplicated attributes.
        // if doc.nodes[start_idx..]
        //     .iter()
        //     .any(|attr| attr.name == attr_name)
        // {
        //     let pos = err_pos_from_qname(text, attr.prefix, attr.local);
        //     return Err(Error::DuplicatedAttribute(attr.local.to_string(), pos));
        // }

        let value_string_idx = string_idx(&mut pd.strings, &mut doc.strings, attr.value.borrow());

        doc.append(
            element_id,
            NodeType::Attribute {
                qname: attr_name_idx,
                value: value_string_idx,
            },
            attr.value_range,
            &mut pd.awaiting_subtree,
        );
    }

    Ok((doc.nodes.len() - start_idx) as u32)
}

fn process_text<'input>(
    text: StrSpan<'input>,
    parent_id: Idx,
    loop_detector: &mut LoopDetector,
    pd: &mut ParserData<'input>,
    doc: &mut Document,
    doc_text: &'input str,
) -> Result<(), Error> {
    // Add text as is if it has only valid characters.
    if !text.as_str().bytes().any(|b| b == b'&' || b == b'\r') {
        append_text(
            Cow::Borrowed(text.as_str()),
            parent_id,
            text.range().into(),
            pd.after_text,
            doc,
            pd,
        );
        pd.after_text = true;
        return Ok(());
    }

    pd.buffer.clear();

    let mut is_as_is = false; // TODO: explain
    let mut s = Stream::from_substr(doc_text, text.range());
    while !s.at_end() {
        match parse_next_chunk(&mut s, &pd.entities)? {
            NextChunk::Byte(c) => {
                if is_as_is {
                    pd.buffer.push_raw(c);
                    is_as_is = false;
                } else {
                    pd.buffer.push_from_text(c, s.at_end());
                }
            }
            NextChunk::Char(c) => {
                for b in CharToBytes::new(c) {
                    if loop_detector.depth > 0 {
                        pd.buffer.push_from_text(b, s.at_end());
                    } else {
                        // Characters not from entity should be added as is.
                        // Not sure why... At least `lxml` produces the same result.
                        pd.buffer.push_raw(b);
                        is_as_is = true;
                    }
                }
            }
            NextChunk::Text(fragment) => {
                is_as_is = false;

                if !pd.buffer.is_empty() {
                    let cow_text = Cow::Owned(pd.buffer.to_str().to_owned());
                    append_text(
                        cow_text,
                        parent_id,
                        text.range().into(),
                        pd.after_text,
                        doc,
                        pd,
                    );
                    pd.after_text = true;
                    pd.buffer.clear();
                }

                loop_detector.inc_references(&s)?;
                loop_detector.inc_depth(&s)?;

                let parser = xmlparser::Tokenizer::from_fragment(doc_text, fragment.range());
                let mut tag_name = TagNameSpan::new_null();
                process_tokens(
                    parser,
                    parent_id,
                    loop_detector,
                    &mut tag_name,
                    pd,
                    doc,
                    doc_text,
                )?;
                pd.buffer.clear();

                loop_detector.dec_depth();
            }
        }
    }

    if !pd.buffer.is_empty() {
        let cow_text = Cow::Owned(pd.buffer.to_str().to_owned());
        append_text(
            cow_text,
            parent_id,
            text.range().into(),
            pd.after_text,
            doc,
            pd,
        );
        pd.after_text = true;
        pd.buffer.clear();
    }

    Ok(())
}

fn append_text<'input>(
    text: Cow<'input, str>,
    parent_id: Idx,
    range: ShortRange,
    after_text: bool,
    doc: &mut Document,
    pd: &mut ParserData<'input>,
) {
    if after_text {
        // Prepend to a previous text node.
        if let Some(node) = doc.nodes.iter_mut().last() {
            if let NodeType::Text(ref mut prev_text_idx) = node.node_type {
                let mut prev_text = doc.strings[prev_text_idx.get_usize()].clone();
                prev_text.push_str(text.borrow());
                *prev_text_idx = string_idx(&mut pd.strings, &mut doc.strings, prev_text.as_str());
            }
        }
    } else {
        let text_string_idx = string_idx(&mut pd.strings, &mut doc.strings, text.borrow());
        doc.append(
            parent_id,
            NodeType::Text(text_string_idx),
            range,
            &mut pd.awaiting_subtree,
        );
    }
}

enum NextChunk<'a> {
    Byte(u8),
    Char(char),
    Text(StrSpan<'a>),
}

fn parse_next_chunk<'a>(
    s: &mut Stream<'a>,
    entities: &[Entity<'a>],
) -> Result<NextChunk<'a>, Error> {
    debug_assert!(!s.at_end());

    // Safe, because we already checked that stream is not at the end.
    // But we have an additional `debug_assert` above just in case.
    let c = s.curr_byte_unchecked();

    // Check for character/entity references.
    if c == b'&' {
        let start = s.pos();
        match s.try_consume_reference() {
            Some(Reference::Char(ch)) => Ok(NextChunk::Char(ch)),
            Some(Reference::Entity(name)) => match entities.iter().find(|e| e.name == name) {
                Some(entity) => Ok(NextChunk::Text(entity.value)),
                None => {
                    let pos = s.gen_text_pos_from(start);
                    Err(Error::UnknownEntityReference(name.into(), pos))
                }
            },
            None => {
                let pos = s.gen_text_pos_from(start);
                Err(Error::MalformedEntityReference(pos))
            }
        }
    } else {
        s.advance(1);
        Ok(NextChunk::Byte(c))
    }
}

// https://www.w3.org/TR/REC-xml/#AVNormalize
fn normalize_attribute<'input>(
    input: &'input str,
    text: StrSpan<'input>,
    entities: &[Entity],
    loop_detector: &mut LoopDetector,
    buffer: &mut TextBuffer,
) -> Result<Cow<'input, str>, Error> {
    if is_normalization_required(&text) {
        buffer.clear();
        _normalize_attribute(input, text, entities, loop_detector, buffer)?;
        Ok(Cow::Owned(buffer.to_str().to_owned()))
    } else {
        Ok(Cow::Borrowed(text.as_str()))
    }
}

#[inline]
fn is_normalization_required(text: &StrSpan) -> bool {
    // We assume that `&` indicates an entity or a character reference.
    // But in rare cases it can be just an another character.

    fn check(c: u8) -> bool {
        match c {
            b'&' | b'\t' | b'\n' | b'\r' => true,
            _ => false,
        }
    }

    text.as_str().bytes().any(check)
}

fn _normalize_attribute(
    input: &str,
    text: StrSpan,
    entities: &[Entity],
    loop_detector: &mut LoopDetector,
    buffer: &mut TextBuffer,
) -> Result<(), Error> {
    let mut s = Stream::from_substr(input, text.range());
    while !s.at_end() {
        // Safe, because we already checked that the stream is not at the end.
        let c = s.curr_byte_unchecked();

        if c != b'&' {
            s.advance(1);
            buffer.push_from_attr(c, s.curr_byte().ok());
            continue;
        }

        // Check for character/entity references.
        let start = s.pos();
        match s.try_consume_reference() {
            Some(Reference::Char(ch)) => {
                for b in CharToBytes::new(ch) {
                    if loop_detector.depth > 0 {
                        // Escaped `<` inside an ENTITY is an error.
                        // Escaped `<` outside an ENTITY is ok.
                        if b == b'<' {
                            return Err(Error::InvalidAttributeValue(s.gen_text_pos_from(start)));
                        }

                        buffer.push_from_attr(b, None);
                    } else {
                        // Characters not from entity should be added as is.
                        // Not sure why... At least `lxml` produces the same results.
                        buffer.push_raw(b);
                    }
                }
            }
            Some(Reference::Entity(name)) => match entities.iter().find(|e| e.name == name) {
                Some(entity) => {
                    loop_detector.inc_references(&s)?;
                    loop_detector.inc_depth(&s)?;
                    _normalize_attribute(input, entity.value, entities, loop_detector, buffer)?;
                    loop_detector.dec_depth();
                }
                None => {
                    let pos = s.gen_text_pos_from(start);
                    return Err(Error::UnknownEntityReference(name.into(), pos));
                }
            },
            None => {
                let pos = s.gen_text_pos_from(start);
                return Err(Error::MalformedEntityReference(pos));
            }
        }
    }

    Ok(())
}

fn get_ns_by_prefix<'input>(
    doc: &Document,
    range: ShortRange,
    prefix: StrSpan,
    doc_text: &'input str,
) -> Result<Option<Cow<'input, str>>, Error> {
    // Prefix CAN be empty when the default namespace was defined.
    //
    // Example:
    // <e xmlns='http://www.w3.org'/>
    let prefix_opt = if prefix.is_empty() {
        None
    } else {
        Some(prefix.as_str())
    };

    let uri = doc.namespaces[range.to_urange()]
        .iter()
        .find(|ns| ns.name.as_str() == prefix_opt)
        .map(|ns| ns.uri.clone());

    match uri {
        Some(v) => Ok(Some(Cow::Owned(v))),
        None => {
            if !prefix.is_empty() {
                // If an URI was not found and prefix IS NOT empty than
                // we have an unknown namespace.
                //
                // Example:
                // <e random:a='b'/>
                let pos = err_pos_from_span(doc_text, prefix);
                Err(Error::UnknownNamespace(prefix.as_str().to_string(), pos))
            } else {
                // If an URI was not found and prefix IS empty than
                // an element or an attribute doesn't have a namespace.
                //
                // Example:
                // <e a='b'/>
                Ok(None)
            }
        }
    }
}

#[inline]
fn gen_qname_string(prefix: Option<&str>, local: &str) -> String {
    if prefix.is_none() {
        local.to_string()
    } else {
        format!("{}:{}", prefix.unwrap(), local)
    }
}

#[inline]
fn err_pos_from_span(input: &str, text: StrSpan) -> TextPos {
    Stream::from_substr(input, text.range()).gen_text_pos()
}

#[inline]
fn err_pos_from_qname(input: &str, prefix: StrSpan, local: StrSpan) -> TextPos {
    let err_span = if prefix.is_empty() { local } else { prefix };
    err_pos_from_span(input, err_span)
}

mod internals {
    /// Iterate over `char` by `u8`.
    pub struct CharToBytes {
        buf: [u8; 4],
        idx: u8,
    }

    impl CharToBytes {
        #[inline]
        pub fn new(c: char) -> Self {
            let mut buf = [0xFF; 4];
            c.encode_utf8(&mut buf);

            CharToBytes { buf, idx: 0 }
        }
    }

    impl Iterator for CharToBytes {
        type Item = u8;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            if self.idx < 4 {
                let b = self.buf[self.idx as usize];

                if b != 0xFF {
                    self.idx += 1;
                    return Some(b);
                } else {
                    self.idx = 4;
                }
            }

            None
        }
    }

    pub struct TextBuffer {
        buf: Vec<u8>,
    }

    impl TextBuffer {
        #[inline]
        pub fn new() -> Self {
            TextBuffer {
                buf: Vec::with_capacity(32),
            }
        }

        #[inline]
        pub fn push_raw(&mut self, c: u8) {
            self.buf.push(c);
        }

        pub fn push_from_attr(&mut self, mut c: u8, c2: Option<u8>) {
            // \r in \r\n should be ignored.
            if c == b'\r' && c2 == Some(b'\n') {
                return;
            }

            // \n, \r and \t should be converted into spaces.
            c = match c {
                b'\n' | b'\r' | b'\t' => b' ',
                _ => c,
            };

            self.buf.push(c);
        }

        // Translate \r\n and any \r that is not followed by \n into a single \n character.
        //
        // https://www.w3.org/TR/xml/#sec-line-ends
        pub fn push_from_text(&mut self, c: u8, at_end: bool) {
            if self.buf.last() == Some(&b'\r') {
                let idx = self.buf.len() - 1;
                self.buf[idx] = b'\n';

                if at_end && c == b'\r' {
                    self.buf.push(b'\n');
                } else if c != b'\n' {
                    self.buf.push(c);
                }
            } else if at_end && c == b'\r' {
                self.buf.push(b'\n');
            } else {
                self.buf.push(c);
            }
        }

        #[inline]
        pub fn clear(&mut self) {
            self.buf.clear();
        }

        #[inline]
        pub fn is_empty(&self) -> bool {
            self.buf.is_empty()
        }

        #[inline]
        pub fn to_str(&self) -> &str {
            use std::str;

            // `unwrap` is safe, because buffer must contain a valid UTF-8 string.
            str::from_utf8(&self.buf).unwrap()
        }
    }
}

use self::internals::*;
use crate::option_ext::OptionExt;
use std::rc::Rc;
