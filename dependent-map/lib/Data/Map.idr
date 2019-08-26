module Data.Map

%default total

export
data Map  : (key : Type) -> (VF : key -> Type) -> Type where
  MNil    : .{VF : key -> Type} -> Map key VF
  MBranch : Ord key => .{VF : key -> Type}
                    -> (k   : key)
                    -> (val : VF  k     )
                    -> (l   : Map key VF)
                    -> (r   : Map key VF)
                    -> Map key VF

%name Map map, map2, map3

empty : Map k p
empty = MNil

insert : Ord key => (k : key) -> p k -> Map key p -> Map key p
insert k x MNil = MBranch k x MNil MNil
insert k x (MBranch kr val l r) =
  case compare k kr of
    LT => MBranch kr val (insert k x l) r
    EQ => MBranch k x l r
    GT => MBranch kr val l (insert k x r)

lookup : Ord key => (k : key) -> Map key p -> Maybe (p k)
lookup _ MNil = Nothing
lookup k (MBranch kr val l r) =
  case compare k kr of
    LT => lookup k l
    GT => lookup k r
    EQ => ?rhs
