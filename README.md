
tiktoken.hs
===========

This library is a binding to an _extremely_ (as in, one function) subset of the
`tiktoken-rs` library. It exposes a function `countTokens :: Text -> Word64` which
can be used to count tokens and return a result which should match the one returned
by OpenAI itself (see for example [their online tool](https://platform.openai.com/tokenizer)).

## Library design

This library uses the [haskell-foreign-rust](https://github.com/BeFunctional/haskell-foreign-rust)
and [haskell-rust-ffi](https://github.com/BeFunctional/haskell-rust-ffi) to call into [tiktoken-rs](https://github.com/zurawiki/tiktoken-rs)
which is currently the industry-standard for tokenisation. Internally, this library is really composed
by a Rust wrapper and a Haskell library, where the former is shipped alongside the latter, and we use
a Custom setup script to seamlessly build the Rust wrapper before building the Haskell library.

For more information see the blog post [Calling Purgatory from Heaven](https://well-typed.com/blog/2023/03/purgatory/).

## Building the project

This project requires a `nighly` version of the Rust toolchain as well as the `cargo-c` applet. You can
install both with:

```
rustup toolchain install nightly
cargo install cargo-c
```

Then, you can build this project like any other Haskell library with `cabal v2-build`.
