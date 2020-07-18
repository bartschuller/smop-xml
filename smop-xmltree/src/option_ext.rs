pub trait OptionExt {
    fn as_str(&self) -> Option<&str>;
}

impl OptionExt for Option<String> {
    #[inline]
    fn as_str(&self) -> Option<&str> {
        match self {
            None => None,
            Some(s) => Some(s.as_str()),
        }
    }
}

impl OptionExt for Option<&String> {
    #[inline]
    fn as_str(&self) -> Option<&str> {
        match self {
            None => None,
            Some(s) => Some(s.as_str()),
        }
    }
}
