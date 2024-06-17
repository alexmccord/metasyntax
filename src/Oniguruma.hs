module Oniguruma where

-- See the RE spec: https://github.com/kkos/oniguruma/blob/master/doc/RE.
-- We implement a parser purely to validate that the regular expression that
-- we generate are conformant to oniguruma's regular expression engine.
--
-- Oniguruma is the regular expression used in the context of syntax highlighting
-- because TextMate started out as using it, and everyone ended up depending on
-- TextMate's `.tmLanguage`, so by transitivity everyone else also used oniguruma.
