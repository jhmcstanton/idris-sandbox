module Data.Map

%default total

export
data Map  : (key : Type) -> (VF : key -> Type) -> Type where
  MNil    : .{VF : key -> Type} -> Map key VF
  MBranch : (DecEq key, Ord key) =>
                       .{VF : key -> Type}
                    -> (k   : key)
                    -> (val : VF  k     )
                    -> (l   : Map key VF)
                    -> (r   : Map key VF)
                    -> Map key VF

%name Map map, map2, map3

empty : Map k p
empty = MNil

insert : (DecEq key, Ord key) => (k : key) -> p k -> Map key p -> Map key p
insert k x MNil = MBranch k x MNil MNil
insert k x (MBranch kr val l r) =
  case compare k kr of
    LT => MBranch kr val (insert k x l) r
    EQ => MBranch k x l r
    GT => MBranch kr val l (insert k x r)

lookup : (DecEq key, Ord key) => (k : key) -> Map key p -> Maybe (p k)
lookup _ MNil = Nothing
lookup k (MBranch kr val l r) =
  case decEq k kr of
    Yes prf => rewrite prf in Just val
    No  _   => case compare k kr of
                 LT => lookup k l
                 _  => lookup k r

insertWith : (DecEq key, Ord key) => (k : key) -> p k -> (p k -> p k -> p k) -> Map key p -> Map key p
insertWith k x f m =
  case lookup k m of
    Just v => insert k (f x v) m
    Nothing => insert k x m
-- it would be nice to implement this with one traversal,
-- but having a hard time rewriting some types
-- insertWith k x f MNil = MBranch k x MNil MNil
-- insertWith {p} k x f (MBranch kr val l r) =
--   case decEq kr k of
--     Yes prf => MBranch k (f x val) l r
--     No  _   => case compare k kr of
--                  LT => insertWith k x f l
--                  _  => insertWith k x f r

fromList : (DecEq key, Ord key) => List (k : key ** p k) -> Map key p
fromList [] = MNil
fromList ((k ** v) :: xs) = insert k v $ fromList xs
