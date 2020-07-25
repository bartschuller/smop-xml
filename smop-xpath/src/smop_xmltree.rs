use smop_xmltree::nod::Node;
use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct AxisIter {
    node: Option<Node>,
    next: fn(&Node) -> Option<Node>,
    pub(crate) position: usize,
    // can be computed lazily only when needed
    last: Option<usize>,
}

impl Iterator for AxisIter {
    type Item = Node;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node.take();
        self.node = node.as_ref().and_then(self.next);
        node
    }
}

impl Debug for AxisIter {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl PartialEq for AxisIter {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}
#[cfg(test)]
mod tests {
    use crate::smop_xmltree::AxisIter;
    use crate::xdm::XdmResult;
    use smop_xmltree::nod::Document;

    #[test]
    fn iterators1() -> XdmResult<()> {
        let doc = Document::parse(r#"<d><e>1</e><e>2</e></d>"#)?;
        let root = doc.root();
        let mut d = root.children();
        let d = d.next().unwrap();
        assert_eq!(d.node_name().unwrap().name.as_str(), "d");
        let _es = d.children().enumerate();
        let _attrs = d.attributes();
        Ok(())
    }

    #[test]
    fn iterators2() -> XdmResult<()> {
        let doc = Document::parse(r#"<d><e>1</e><e>2</e></d>"#)?;
        let root = doc.root();
        let _children = AxisIter {
            node: None,
            next: |_n| None,
            position: 0,
            last: None,
        };
        let mut d = root.children();
        let d = d.next().unwrap();
        assert_eq!(d.node_name().unwrap().name.as_str(), "d");
        let _es = d.children().enumerate();

        Ok(())
    }
}
