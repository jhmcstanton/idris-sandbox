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

-- rewrite with absurd
-- insertWith : (DecEq key, Ord key) => (p k -> p k -> p k) -> (k : key) -> p k -> Map key p -> Maybe (Map key p)
-- insertWith f k x MNil = Just $ MBranch k x MNil MNil
-- insertWith f k x (MBranch kr val l r) =
--   case compare k kr of
--     LT => map (\l' => MBranch kr val l' r) $ insertWith f k x l
--     GT => map (\r' => MBranch kr val l r') $ insertWith f k x r
--     EQ => case decEq k kr of
--             Yes prf   => Just $ MBranch kr (rewrite cong prf in f x val) l r
--             No contra => Nothing

-- rewrite with absurd
lookup : (DecEq key, Ord key) => (k : key) -> Map key p -> Maybe (p k)
lookup _ MNil = Nothing
lookup k (MBranch kr val l r) =
  case compare k kr of
    LT => lookup k l
    GT => lookup k r
    EQ => case decEq k kr of
            (Yes prf)   => rewrite prf in Just val
            (No contra) => Nothing -- should never he possible

fromList : (DecEq key, Ord key) => List (k : key ** p k) -> Map key p
fromList [] = MNil
fromList ((k ** v) :: xs) = insert k v $ fromList xs

