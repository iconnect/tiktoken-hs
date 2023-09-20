module Data.Tokenizer.BPE where

#include "tiktoken_rs_hs_wrapper.h"

import Data.Word
import Data.Text
import Foreign.Rust.Marshall.Variable

{# fun pure unsafe count_bpe_tokens as countTokens
     { toBorshVar* `Text'&
     }
  -> `Word64'
#}
