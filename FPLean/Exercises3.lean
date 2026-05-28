-- Exercises 3.1.6.1

-- Replace
-- inductive Pos : Type where
--   | one : Pos
--   | succ : Pos → Pos
-- with
structure Pos where
  succ ::
  pred : Nat

def Pos.Add : Pos -> Pos -> Pos
  | a, Pos.succ Nat.zero => Pos.succ a.pred
  | a, Pos.succ b => Pos.succ (a.pred + b + 1)

instance : Add Pos where
  add := Pos.Add

def Pos.mul : Pos -> Pos -> Pos
  | a, Pos.succ Nat.zero => a
  | a, Pos.succ b => Pos.succ ((a.pred + 1) * (b + 1) - 1)

instance : Mul Pos where
  mul := Pos.mul

instance : ToString Pos where
  toString p := toString (p.pred + 1)

instance : OfNat Pos n where
  ofNat := Pos.succ (n - 1)

def three : Pos := Pos.succ 2
def two : Pos := Pos.succ 1
def one : Pos := Pos.succ 0

#eval s!"1={one}, 2={two}: 3+2={three + two}, 3*2={three * two}"

-- Exercises 3.1.6.2

-- The nth even number is repr by half=n/2.
structure Even where
  half : Nat

def Even.Add : Even -> Even -> Even
  | a, b => Even.mk (a.half + b.half)

instance : Add Even where
  add := Even.Add

def Even.mul : Even -> Even -> Even
  | a, b => Even.mk (2 * a.half * b.half)

instance : Mul Even where
  mul := Even.mul

instance : OfNat Even n where
  ofNat := Even.mk (n / 2)

instance : ToString Even where
  toString e := s!"{2 * e.half}"

def esix : Even := Even.mk 3
def efour : Even := Even.mk 2
def etwo : Even := Even.mk 1

#eval s!"2={etwo}, 4={efour}, 6={esix}: 2+4={etwo + efour}, 2*4={etwo * efour}"

-- Exercise 3.1.6.3
-- TODO


-- Exercise 3.5.9

-- HAppend for NonEmptyList
structure NonEmptyList (α : Type) where
  head : α
  tail : List α
deriving Repr
def hAppend {α : Type} (l1 l2 : NonEmptyList α) : NonEmptyList α :=
  NonEmptyList.mk l1.head (l1.tail ++ (l2.head :: l2.tail))
instance : HAppend (NonEmptyList α) (NonEmptyList α) (NonEmptyList α) where
  hAppend := hAppend

-- Functor.map for BinTree
inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α
deriving Repr

def map {α β : Type} (f : α → β) : BinTree α → BinTree β
| BinTree.leaf => BinTree.leaf
| BinTree.branch l x r => BinTree.branch (map f l) (f x) (map f r)

instance : Functor BinTree where
  map := map
-- Directly defining the `map` inside the Functor instance runs into
-- typing issues.




inductive A where
  | a

inductive B where
  | b

instance : Coe A B where
  coe _ := B.b

instance : Coe B A where
  coe _ := A.a

instance : Coe Unit A where
  coe _ := A.a

def coercedToB : B := ()

#eval coercedToB
