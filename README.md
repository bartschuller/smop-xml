# Rust XML libraries

These are the beginnings of something that could become an XPath 3.1 implementation in Rust.
It is alpha-quality software.

## How to use

- `git submodule update --init`
- Install Rust stable using `rustup`
- `cd smop-qt3tester`
- `cargo run -- catalog.xml` This will run tests from the XQuery/XPath test suite.
- edit `catalog.xml` to enable other tests, or add new ones from `qt3tests/catalog.xml`
- `smop-xpath` is the main library. The API is unstable
- `smop-xmltree` is the XML document model that's used by `smop-xpath`. It is derived from [`roxmltree`](https://github.com/RazrFalcon/roxmltree).

## References

- https://www.w3.org/TR/xpath-31/
- https://www.w3.org/TR/xpath-functions-31/
- https://www.w3.org/TR/xpath-datamodel-31/
- https://www.w3.org/TR/xquery-semantics/ (not a standard, still useful)
- https://www.w3.org/TR/xmlschema-1/
- https://www.w3.org/TR/xmlschema-2/
- https://www.w3.org/TR/xmlschema11-2/

## Current musings

The runtime data model (`smop_xpath::xdm`) is currently an enum and doesn't store runtime types. The compile time types also don't do much. I don't know what the best data structure for `xdm` items would be. enum? structs? traits? some combination of those?

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

## Authors

Bart Schuller

The name SMOP (small matter of programming) came about because I still own the smop.org domain, which doesn't run a website at the moment.
