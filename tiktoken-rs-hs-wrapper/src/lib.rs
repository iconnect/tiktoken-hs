use haskell_ffi::from_haskell::marshall_from_haskell_var;
use std::marker::PhantomData;
use tiktoken_rs::p50k_base;

pub enum TiktokenHs {}
pub const TiktokenHs: PhantomData<TiktokenHs> = PhantomData;

#[no_mangle]
pub extern "C" fn count_bpe_tokens(prompt: *const u8, prompt_len: usize) -> usize {
    let prompt: String = marshall_from_haskell_var(prompt, prompt_len, TiktokenHs);

    let bpe = p50k_base().unwrap();
    let tokens = bpe.encode_with_special_tokens(&prompt);
    return tokens.len();
}
