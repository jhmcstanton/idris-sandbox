module Text.RE

import Data.List.Views

%default total

data RE =
    ROnce  String  -- literals
  | R0Plus RE      -- *
  | R1Plus RE      -- +
  | ROpt   RE      -- ?
  | RMerge RE RE   -- compound RE
  | RBeg           -- EOF

data REParseError = REError String

r0plus : RE -> RE
r0plus (RMerge l r) = RMerge l (R0Plus r)
r0plus r            = R0Plus r

r1plus : RE -> RE
r1plus (RMerge l r) = RMerge l (R1Plus r)
r1plus r            = R1Plus r

ropt : RE -> RE
ropt (RMerge l r) = RMerge l (ROpt r)
ropt r            = ROpt r

rtokens : List Char
rtokens = unpack "*+?()"

-- TODO: Make this total
format : List Char -> Either REParseError RE
format xs with (snocList xs)
  format [] | Empty = Right RBeg
  format (ys ++ [c]) | (Snoc rs) with (snocList ys)
    format ([] ++ [c]) | (Snoc rs) | Empty =
      if elem c rtokens
      then Left . REError $ "Missing expression for quantifier: " ++ singleton c
      else Right (RMerge RBeg (ROnce (singleton c)))
    format ((xs ++ [x]) ++ [c]) | (Snoc rs) | (Snoc rec) =
      case c of
        '*' => map r0plus (format (xs ++ [x]) | rs)
        '+' => map r1plus (format (xs ++ [x]) | rs)
        '?' => map ropt   (format (xs ++ [x]) | rs)
        ')' => do let (this, next) = span (/= '(') (xs ++ [x])
                  prev <- format next -- not total here
                  t    <- format this
                  pure (RMerge prev t)
        '(' => Left . REError $ "Mismatched paren, extra ("
        c   => case format (xs ++ [x] | rs) of
                 Left e          => Left e
                 Right (ROnce s) => Right (ROnce (s ++ singleton c))
                 Right w         => Right (RMerge w (ROnce (singleton c)))

formatStr : String -> Either REParseError RE
formatStr = format . unpack
