module Data.Tokenizer.BPE where

#include "tiktoken_rs_hs_wrapper.h"

import Data.Word

{# fun pure unsafe count_bpe_tokens as bpeCountTokens
     { `Word64'
     }
  -> `Word64'
#}
