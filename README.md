
tiktoken.hs
===========

This library is a binding to an _extremely_ (as in, one function) subset of the
`tiktoken-rs` library. It exposes a function `countTokens :: Text -> Word64` which
can be used to count tokens and return a result which should match the one returned
by OpenAI itself (see for example [their online tool](https://platform.openai.com/tokenizer)).
