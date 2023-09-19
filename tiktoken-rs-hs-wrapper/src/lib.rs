use tiktoken_rs::p50k_base;

#[no_mangle]
pub extern "C" fn count_bpe_tokens(_x: u64) -> usize {
    let bpe = p50k_base().unwrap();
    let tokens = bpe.encode_with_special_tokens("This is a test         with a lot of spaces");
    return tokens.len();
}
