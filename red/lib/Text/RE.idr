module Text.RE

import Data.List.Views

%default total

data RE =
    RLit   Char    -- literals
  | R0Plus RE      -- *
  | R1Plus RE      -- +
  | ROpt   RE      -- ?
  | RConcat RE RE  -- <expr><expr>
  | RAlt RE RE     -- <expr>|<expr>

data REParseError = REError String

rtokens : List Char
rtokens = unpack "*+?()|"

format : List Char -> Either REParseError RE

formatStr : String -> Either REParseError RE
formatStr = format . unpack
