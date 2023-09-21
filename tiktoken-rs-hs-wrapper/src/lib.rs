use haskell_ffi::from_haskell::marshall_from_haskell_var;
use haskell_ffi::to_haskell::marshall_to_haskell_var;
use std::marker::PhantomData;
use tiktoken_rs::p50k_base;

pub enum TiktokenHs {}
pub const TIKTOKEN_HS: PhantomData<TiktokenHs> = PhantomData;

#[no_mangle]
pub extern "C" fn count_bpe_tokens(prompt: *const u8, prompt_len: usize) -> usize {
    let prompt: String = marshall_from_haskell_var(prompt, prompt_len, TIKTOKEN_HS);

    let bpe = p50k_base().unwrap();
    let tokens = bpe.encode_with_special_tokens(&prompt);
    return tokens.len();
}

#[no_mangle]
pub extern "C" fn split_by_token(
    prompt: *const u8,
    prompt_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let prompt: String = marshall_from_haskell_var(prompt, prompt_len, TIKTOKEN_HS);
    let bpe = p50k_base().unwrap();
    let tokenized: Result<Vec<_>, _> = bpe.split_by_token_iter(&prompt, true).collect();
    let tokens = tokenized.unwrap();
    marshall_to_haskell_var(&tokens, out, out_len, TIKTOKEN_HS);
}
