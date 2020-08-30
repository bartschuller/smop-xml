use smop_xmltree::nod::Node;

#[derive(Clone)]
pub struct AxisIter {
    pub(crate) node: Option<Node>,
    pub(crate) next: fn(&Node) -> Option<Node>,
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
        };
        let mut d = root.children();
        let d = d.next().unwrap();
        assert_eq!(d.node_name().unwrap().name.as_str(), "d");
        let _es = d.children().enumerate();

        Ok(())
    }
}
