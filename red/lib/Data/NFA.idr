module Data.NFA

%default total
%access public export

record NFA s elem where
  constructor MkNFA
  init       : s
  transition : s -> elem -> List s
  terminal   : s -> Bool

accepts : Eq elem => NFA s elem -> List elem -> Bool
accepts (MkNFA init transition terminal) = any terminal . foldlM transition init

data State : Type -> Type where
  Match  :                                                State elem
  Split  : (left : State elem) -> (right : State elem) -> State elem
  Single : (out  : elem)       -> (next  : State elem) -> State elem

data Expression : Type -> Type where
  ELit : elem                                                                      -> Expression elem
  EUn  : (opname : String) -> (operation : State elem -> State elem)               -> Expression elem
  EBin : (opname : String) -> (operation : State elem -> State elem -> State elem) -> Expression elem

data ExprCompilationError = ECompErr String

private
compErr : String -> Either ExprCompilationError a
compErr = Left . ECompErr

-- compile : List (Expression elem) -> Either ExprCompilationError (State elem)
-- compile xs = go xs [] where
--   go : List (Expression elem) -> List (Frag elem) -> Either ExprCompilationError (State elem)
--   go [] [] = compErr "Nothing to compile"
--   go [] (s :: []) = case state s of
--                       Ready x     => Right x
--                       TempSplit _ =>
--                         compErr "Compilation error, state value not complete. Possibly a bug in the implementation."
--   go [] (s :: ss) = compErr "Compilation error, extra values remaining in stack. Probably a bug in the implementation."
--   go (ELit l :: xs) ys = go xs (MkFrag (Ready $ Single l) Nil :: ys)
--   go (EUn _ op :: xs) (o1 :: ys) = go xs (op o1 :: ys)
--   go (EUn name _ :: xs) ys       = compErr $ "Compilation error: operation [ " ++ name ++ " ] requires an operand."
--   go (EBin _ op :: xs) (o1 :: o2 :: ys) = go xs (op o1 o2 :: ys)
--   go (EBin name _ :: xs) ys = compErr $ "Compilation error: operation [ " ++ name ++ " ] requires two operands."
