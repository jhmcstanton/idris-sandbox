module Data.NFA

%default total
%access public export

data State : Type -> Type where
  Match  : State elem
  Split  : (left : State elem) -> (right : State elem) -> State elem
  Single : (out  : elem) -> State elem

record Frag elem where
  constructor MkFrag
  state : State elem
  nextStates : List (State elem)

data Expression : Type -> Type where
  ELit : elem -> Expression elem
  EPop : (opname : String) -> (operation : Frag elem -> Frag elem -> Frag elem) -> Expression elem

data ExprCompilationError = ECompErr String

private
compErr : String -> Either ExprCompilationError a
compErr = Left . ECompErr

compile : List (Expression elem) -> Either ExprCompilationError (State elem)
compile xs = go xs [] where
  go : List (Expression elem) -> List (Frag elem) -> Either ExprCompilationError (State elem)
  go [] [] = compErr "Nothing to compile"
  go [] (s :: []) = Right (state s)
  go [] (s :: ss) = compErr "Compilation error, extra values remaining in stack. Probably a bug in the implementation."
  go (ELit l :: xs) ys = go xs (MkFrag (Single l) Nil :: ys)
  go (EPop _ op :: xs) (o1 :: o2 :: ys) = go xs (op o1 o2 :: ys)
  go (EPop name _ :: xs) ys = compErr $ "Compilation error: operation [ " ++ name ++ " ] requires at least two operands."
